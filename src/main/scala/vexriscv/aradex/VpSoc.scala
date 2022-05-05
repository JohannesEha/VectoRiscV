package vexriscv.aradex

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.simple.PipelinedMemoryBus
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.spi.ddr.SpiXdrMaster
import spinal.lib.com.uart._
import spinal.lib.io.{InOutWrapper, TriStateArray}
import spinal.lib.misc.{InterruptCtrl, Prescaler, Timer}
import spinal.lib.soc.pinsec.{PinsecTimerCtrl, PinsecTimerCtrlExternal}
import vexriscv.plugin._
import vexriscv.{VexRiscv, VexRiscvConfig, plugin}
import spinal.lib.com.spi.ddr._
import spinal.lib.bus.simple._
import spinal.lib.com.i2c.{I2c, Apb3I2cCtrl, I2cMasterMemoryMappedGenerics, I2cSlaveGenerics, I2cSlaveMemoryMappedGenerics}
import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq

case class AradexConfig(coreFrequency : HertzNumber,
                       onChipRamSize      : BigInt,
					   dpRamSize          : BigInt,
                       onChipRamHexFile   : String,
                       pipelineDBus       : Boolean,
                       pipelineMainBus    : Boolean,
                       pipelineApbBridge  : Boolean,
                       pipelineUserIntf   : Boolean,
                       uartCtrlConfig     : UartCtrlMemoryMappedConfig,
                       hardwareBreakpointCount : Int,
                       cpuPlugins         : ArrayBuffer[Plugin[VexRiscv]]){
  require(pipelineApbBridge || pipelineMainBus, "At least pipelineMainBus or pipelineApbBridge should be enable to avoid wipe transactions")
}


object AradexConfig{
  def default : AradexConfig = default(false)
  def default(bigEndian : Boolean = false) =  AradexConfig(
    coreFrequency         = 80 MHz,
    onChipRamSize         = 48 kB,
    dpRamSize             = 8 kB,
    onChipRamHexFile      = "vp_app.hex",
    pipelineDBus          = true,
    pipelineMainBus       = false,
    pipelineApbBridge     = true,
    pipelineUserIntf      = true,
    hardwareBreakpointCount = 0,
    cpuPlugins = ArrayBuffer( //DebugPlugin added by the toplevel
      new IBusSimplePlugin(
        resetVector = 0x10000l,
        cmdForkOnSecondStage = true,
        cmdForkPersistence = false, //Required by the Xip controller
        prediction = NONE,
        catchAccessFault = false,
        compressedGen = false,
        bigEndian = bigEndian
      ),
      new DBusSimplePlugin(
        catchAddressMisaligned = false,
        catchAccessFault = false,
        earlyInjection = false,
        bigEndian = bigEndian
      ),
      new CsrPlugin(CsrPluginConfig.smallest(mtvecInit = 0x10020l)),
      new DecoderSimplePlugin(
        catchIllegalInstruction = false
      ),
      new RegFilePlugin(
        regFileReadyKind = plugin.SYNC,
        zeroBoot = false
      ),
      new IntAluPlugin,
      new SrcPlugin(
        separatedAddSub = false,
        executeInsertion = false
      ),
      new FullBarrelShifterPlugin,
	  new MulPlugin,
	  new DivPlugin,
	  // new MulDivIterativePlugin(
        // genMul = true,
        // genDiv = false,
        // mulUnrollFactor = 1,
        // divUnrollFactor = 1
      // ),
      new HazardSimplePlugin(
        bypassExecute = false,			// true for higher speed
        bypassMemory = false,			// true
        bypassWriteBack = false,		// true
        bypassWriteBackBuffer = false,	// true
        pessimisticUseSrc = false,
        pessimisticWriteRegFile = false,
        pessimisticAddressMatch = false
      ),
      new BranchPlugin(
        earlyBranch = false,
        catchAddressMisaligned = false
      ),
      new YamlPlugin("cpu0.yaml")
    ),
    uartCtrlConfig = UartCtrlMemoryMappedConfig(
      uartCtrlConfig = UartCtrlGenerics(
        dataWidthMax      = 8,
        clockDividerWidth = 20,
        preSamplingSize   = 1,
        samplingSize      = 3,
        postSamplingSize  = 1
      ),
      initConfig = UartCtrlInitConfig(
        baudrate = 115200,
        dataLength = 7,  //7 => 8 bits
        parity = UartParityType.NONE,
        stop = UartStopType.ONE
      ),
      busCanWriteClockDividerConfig = false,
      busCanWriteFrameConfig = false,
      txFifoDepth = 16,
      rxFifoDepth = 16
    )
  )
}

object VpSoc {
  val apb3Config = Apb3Config(
    addressWidth = 16,
    dataWidth = 32
  )

  val userIntfConfig = UserIntfConfig(
    addressWidth = 10,
    dataWidth = 32
  )

  val dpramIntfConfig = DpramIntfConfig(
    addressWidth = 11,
    dataWidth = 32
  )
}


case class VpSoc(config : AradexConfig) extends Component{
  import config._

  val io = new Bundle {
    //Clocks / reset
    val asyncReset = in Bool()
    val mainClk = in Bool()

    //Main components IO
    val jtag = slave(Jtag())

    //Peripherals IO
    val uart = master(Uart())

    //i2c interface
    val i2c = master(I2c())

    //User interface
    val userIntf = master(UserIntf(VpSoc.userIntfConfig))
	val extIRQ = in Bool()
	
	//DPRAM
    val dpramIntf = slave(DpramInterface(VpSoc.dpramIntfConfig))
}


  val resetCtrlClockDomain = ClockDomain(
    clock = io.mainClk,
    config = ClockDomainConfig(
      resetKind = BOOT
    )
  )

  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    val mainClkResetUnbuffered  = False

    //Implement an counter to keep the reset axiResetOrder high 64 cycles
    // Also this counter will automatically do a reset when the system boot.
    val systemClkResetCounter = Reg(UInt(6 bits)) init(0)
    when(systemClkResetCounter =/= U(systemClkResetCounter.range -> true)){
      systemClkResetCounter := systemClkResetCounter + 1
      mainClkResetUnbuffered := True
    }
    when(BufferCC(io.asyncReset)){
      systemClkResetCounter := 0
    }

    //Create all reset used later in the design
    val mainClkReset = RegNext(mainClkResetUnbuffered)
    val systemReset  = RegNext(mainClkResetUnbuffered)
  }


  val systemClockDomain = ClockDomain(
    clock = io.mainClk,
    reset = resetCtrl.systemReset,
    frequency = FixedFrequency(coreFrequency)
  )

  val debugClockDomain = ClockDomain(
    clock = io.mainClk,
    reset = resetCtrl.mainClkReset,
    frequency = FixedFrequency(coreFrequency)
  )

  val system = new ClockingArea(systemClockDomain) {
    val pipelinedMemoryBusConfig = PipelinedMemoryBusConfig(
      addressWidth = 32,
      dataWidth = 32
    )

    val bigEndianDBus = config.cpuPlugins.exists(_ match{ case plugin : DBusSimplePlugin => plugin.bigEndian case _ => false})

    //Arbiter of the cpu dBus/iBus to drive the mainBus
    //Priority to dBus, !! cmd transactions can change on the fly !!
    val mainBusArbiter = new AradexMasterArbiter(pipelinedMemoryBusConfig, bigEndianDBus)

    //Instanciate the CPU
    val cpu = new VexRiscv(
      config = VexRiscvConfig(
        plugins = cpuPlugins += new DebugPlugin(debugClockDomain, hardwareBreakpointCount)
      )
    )

    //Checkout plugins used to instanciate the CPU to connect them to the SoC
    val timerInterrupt = False
    val externalInterrupt = False
    for(plugin <- cpu.plugins) plugin match{
      case plugin : IBusSimplePlugin =>
        mainBusArbiter.io.iBus.cmd <> plugin.iBus.cmd
        mainBusArbiter.io.iBus.rsp <> plugin.iBus.rsp
      case plugin : DBusSimplePlugin => {
        if(!pipelineDBus)
          mainBusArbiter.io.dBus <> plugin.dBus
        else {
          mainBusArbiter.io.dBus.cmd << plugin.dBus.cmd.halfPipe()
          mainBusArbiter.io.dBus.rsp <> plugin.dBus.rsp
        }
      }
      case plugin : CsrPlugin        => {
        plugin.externalInterrupt := externalInterrupt
        plugin.timerInterrupt := timerInterrupt
      }
      case plugin : DebugPlugin         => plugin.debugClockDomain{
        resetCtrl.systemReset setWhen(RegNext(plugin.io.resetOut))
        io.jtag <> plugin.io.bus.fromJtag()
      }
      case _ =>
    }

    //****** MainBus slaves ********
    val mainBusMapping = ArrayBuffer[(PipelinedMemoryBus,SizeMapping)]()
    val ramStartAddress = 0x00010000l
    val ram = new AradexPipelinedMemoryBusRam(
      onChipRamSize = onChipRamSize,
      onChipRamHexFile = onChipRamHexFile,
      pipelinedMemoryBusConfig = pipelinedMemoryBusConfig,
      ramStartAddress,
      bigEndian = bigEndianDBus
    )
    mainBusMapping += ram.io.bus -> (ramStartAddress, onChipRamSize)

    val apbBridge = new PipelinedMemoryBusToApbBridge(
      VpSoc.apb3Config,
      pipelineBridge = pipelineApbBridge,
      pipelinedMemoryBusConfig = pipelinedMemoryBusConfig
    )
    mainBusMapping += apbBridge.io.pipelinedMemoryBus -> (0x00020000l, 64 kB)

	// user interface
	val userInterface = PipelinedMemoryBusUserInterface(
	  userIntfConfig = VpSoc.userIntfConfig, 
	  pipelineBridge = pipelineUserIntf,
	  pipelinedMemoryBusConfig = pipelinedMemoryBusConfig
    )
	mainBusMapping += userInterface.io.pipelinedMemoryBus -> (0x00030000l, 1 kB)

	//DPRAM
	val dpram = AradexPipelinedMemoryBusDPRam(
	  dpramIntfConfig = VpSoc.dpramIntfConfig,
      dpRamSize = dpRamSize,
      pipelinedMemoryBusConfig = pipelinedMemoryBusConfig,
      bigEndian = bigEndianDBus
	)
	mainBusMapping += dpram.io.bus -> (0x00040000l, dpRamSize)

    //******** APB peripherals *********
    val apbMapping = ArrayBuffer[(Apb3, SizeMapping)]()

    val uartCtrl = Apb3UartCtrl(uartCtrlConfig)
    uartCtrl.io.uart <> io.uart
    apbMapping += uartCtrl.io.apb  -> (0x2000, 4 kB)

    val timer = new AradexApb3Timer()
    timerInterrupt setWhen(timer.io.interrupt)
    apbMapping += timer.io.apb     -> (0x4000, 4 kB)
    
    val apb3I2cCtrl = new Apb3I2cCtrl(
      I2cSlaveMemoryMappedGenerics(
        ctrlGenerics = I2cSlaveGenerics(
          samplingWindowSize = 3,
          samplingClockDividerWidth = 10 bits,
          timeoutWidth = 20 bits
        ),
        addressFilterCount = 4,
        masterGenerics = I2cMasterMemoryMappedGenerics(
          timerWidth = 16
        )
      )
    )
    apbMapping += apb3I2cCtrl.io.apb -> (0x5000, 1 kB)

    //******** Memory mappings *********
    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = apbMapping.toSeq
    )

    val mainBusDecoder = new Area {
      val logic = new AradexPipelinedMemoryBusDecoder(
        master = mainBusArbiter.io.masterBus,
        specification = mainBusMapping.toSeq,
        pipelineMaster = pipelineMainBus
      )
    }
	
	externalInterrupt setWhen io.extIRQ
	io.userIntf <> userInterface.io.userIntf
	io.dpramIntf <> dpram.io.dpram
    io.i2c <> apb3I2cCtrl.io.i2c
  }
}

object VpSocVhdl{
  def main(args: Array[String]) {
    SpinalVhdl(VpSoc(AradexConfig.default))
  }
}

object VpSocVerilog{
  def main(args: Array[String]) {
    SpinalVerilog(VpSoc(AradexConfig.default))
  }
}
