package vexriscv.aradex

import java.nio.{ByteBuffer, ByteOrder}

import spinal.core._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.misc.{HexTools, InterruptCtrl, Prescaler, Timer}
import spinal.lib._
import spinal.lib.bus.simple._
import vexriscv.plugin.{DBusSimpleBus, IBusSimpleBus}

class AradexMasterArbiter(pipelinedMemoryBusConfig : PipelinedMemoryBusConfig, bigEndian : Boolean = false) extends Component{
  val io = new Bundle{
    val iBus = slave(IBusSimpleBus(null))
    val dBus = slave(DBusSimpleBus(bigEndian))
    val masterBus = master(PipelinedMemoryBus(pipelinedMemoryBusConfig))
  }

  io.masterBus.cmd.valid   := io.iBus.cmd.valid || io.dBus.cmd.valid
  io.masterBus.cmd.write      := io.dBus.cmd.valid && io.dBus.cmd.wr
  io.masterBus.cmd.address := io.dBus.cmd.valid ? io.dBus.cmd.address | io.iBus.cmd.pc
  io.masterBus.cmd.data    := io.dBus.cmd.data
  io.masterBus.cmd.mask    := io.dBus.genMask(io.dBus.cmd)
  io.iBus.cmd.ready := io.masterBus.cmd.ready && !io.dBus.cmd.valid
  io.dBus.cmd.ready := io.masterBus.cmd.ready


  val rspPending = RegInit(False) clearWhen(io.masterBus.rsp.valid)
  val rspTarget = RegInit(False)
  when(io.masterBus.cmd.fire && !io.masterBus.cmd.write){
    rspTarget  := io.dBus.cmd.valid
    rspPending := True
  }

  when(rspPending && !io.masterBus.rsp.valid){
    io.iBus.cmd.ready := False
    io.dBus.cmd.ready := False
    io.masterBus.cmd.valid := False
  }

  io.iBus.rsp.valid := io.masterBus.rsp.valid && !rspTarget
  io.iBus.rsp.inst  := io.masterBus.rsp.data
  io.iBus.rsp.error := False

  io.dBus.rsp.ready := io.masterBus.rsp.valid && rspTarget
  io.dBus.rsp.data  := io.masterBus.rsp.data
  io.dBus.rsp.error := False
}


case class AradexPipelinedMemoryBusRam(onChipRamSize : BigInt, onChipRamHexFile : String, pipelinedMemoryBusConfig : PipelinedMemoryBusConfig, StartAddress : BigInt, bigEndian : Boolean = false) extends Component{
  val io = new Bundle{
    val bus = slave(PipelinedMemoryBus(pipelinedMemoryBusConfig))
  }

  val ram = Mem(Bits(32 bits), onChipRamSize / 4)
  io.bus.rsp.valid := RegNext(io.bus.cmd.fire && !io.bus.cmd.write) init(False)
  io.bus.rsp.data := ram.readWriteSync(
    address = (io.bus.cmd.address >> 2).resized,
    data  = io.bus.cmd.data,
    enable  = io.bus.cmd.valid,
    write  = io.bus.cmd.write,
    mask  = io.bus.cmd.mask
  )
  io.bus.cmd.ready := True

  if(onChipRamHexFile != null){
    HexTools.initRam(ram, onChipRamHexFile, StartAddress)
    if(bigEndian)
      // HexTools.initRam (incorrectly) assumes little endian byte ordering
      for((word, wordIndex) <- ram.initialContent.zipWithIndex)
        ram.initialContent(wordIndex) =
          ((word & 0xffl)       << 24) |
          ((word & 0xff00l)     << 8)  |
          ((word & 0xff0000l)   >> 8)  |
          ((word & 0xff000000l) >> 24)
  }
}


class AradexPipelinedMemoryBusDecoder(master : PipelinedMemoryBus, val specification : Seq[(PipelinedMemoryBus,SizeMapping)], pipelineMaster : Boolean) extends Area{
  val masterPipelined = PipelinedMemoryBus(master.config)
  if(!pipelineMaster) {
    masterPipelined.cmd << master.cmd
    masterPipelined.rsp >> master.rsp
  } else {
    masterPipelined.cmd <-< master.cmd
    masterPipelined.rsp >> master.rsp
  }

  val slaveBuses = specification.map(_._1)
  val memorySpaces = specification.map(_._2)

  val hits = for((slaveBus, memorySpace) <- specification) yield {
    val hit = memorySpace.hit(masterPipelined.cmd.address)
    slaveBus.cmd.valid   := masterPipelined.cmd.valid && hit
    slaveBus.cmd.payload := masterPipelined.cmd.payload.resized
    hit
  }
  val noHit = !hits.orR
  masterPipelined.cmd.ready := (hits,slaveBuses).zipped.map(_ && _.cmd.ready).orR || noHit

  val rspPending  = RegInit(False) clearWhen(masterPipelined.rsp.valid) setWhen(masterPipelined.cmd.fire && !masterPipelined.cmd.write)
  val rspNoHit    = RegNext(False) init(False) setWhen(noHit)
  val rspSourceId = RegNextWhen(OHToUInt(hits), masterPipelined.cmd.fire)
  masterPipelined.rsp.valid   := slaveBuses.map(_.rsp.valid).orR || (rspPending && rspNoHit)
  masterPipelined.rsp.payload := slaveBuses.map(_.rsp.payload).read(rspSourceId)

  when(rspPending && !masterPipelined.rsp.valid) { //Only one pending read request is allowed
    masterPipelined.cmd.ready := False
    slaveBuses.foreach(_.cmd.valid := False)
  }
}


class AradexApb3Timer extends Component{
  val io = new Bundle {
    val apb = slave(Apb3(
      addressWidth = 8,
      dataWidth = 32
    ))
    val interrupt = out Bool()
  }

  val prescaler = Prescaler(16)
  val timerA,timerB = Timer(16)

  val busCtrl = Apb3SlaveFactory(io.apb)
  val prescalerBridge = prescaler.driveFrom(busCtrl,0x00)

  val timerABridge = timerA.driveFrom(busCtrl,0x40)(
    ticks  = List(True, prescaler.io.overflow),
    clears = List(timerA.io.full)
  )

  val timerBBridge = timerB.driveFrom(busCtrl,0x50)(
    ticks  = List(True, prescaler.io.overflow),
    clears = List(timerB.io.full)
  )

  val interruptCtrl = InterruptCtrl(2)
  val interruptCtrlBridge = interruptCtrl.driveFrom(busCtrl,0x10)
  interruptCtrl.io.inputs(0) := timerA.io.full
  interruptCtrl.io.inputs(1) := timerB.io.full
  io.interrupt := interruptCtrl.io.pendings.orR
}

object AradexApb3TimerGen extends App{
  SpinalVhdl(new AradexApb3Timer())
}


case class DpramIntfConfig(
  addressWidth  : Int,
  dataWidth     : Int
)

object DpramInterface{

  def apply(addressWidth: Int, dataWidth: Int) = new DpramInterface(DpramIntfConfig(addressWidth = addressWidth, dataWidth = dataWidth))
}

case class DpramInterface(config: DpramIntfConfig) extends Bundle with IMasterSlave {

  // outputs
  val ADDR    = UInt(config.addressWidth bits)
  val CS      = Bool()
  val WRITE   = Bool()
  val WDATA   = Bits(config.dataWidth bits)
  
  // inputs
  val RDATA   = Bits(config.dataWidth bits)
  
  override def asMaster(): Unit = {
    out(ADDR, CS, WRITE, WDATA)
    in(RDATA)
  }
  
  def << (sink: DpramInterface): Unit = sink >> this

  def >> (sink: DpramInterface): Unit = {
    sink.ADDR   := this.ADDR.resized
    sink.CS     := this.CS
    sink.WRITE  := this.WRITE
    sink.WDATA  := this.WDATA
    this.RDATA  := sink.RDATA
  }
}


case class AradexPipelinedMemoryBusDPRam(dpramIntfConfig : DpramIntfConfig, dpRamSize : BigInt, pipelinedMemoryBusConfig : PipelinedMemoryBusConfig, bigEndian : Boolean = false) extends Component{
  val io = new Bundle{
    val bus = slave(PipelinedMemoryBus(pipelinedMemoryBusConfig))
	val dpram = slave(DpramInterface(dpramIntfConfig))
  }

  val a = new Area{
    val ram = Mem(Bits(32 bits), dpRamSize / 4)
    ram.addAttribute("ramstyle", "no_rw_check")

    io.bus.rsp.valid := RegNext(io.bus.cmd.fire && !io.bus.cmd.write) init(False)
    io.bus.rsp.data := ram.readWriteSync(
      address = (io.bus.cmd.address >> 2).resized,
      data  = io.bus.cmd.data,
      enable  = True,
      write  = io.bus.cmd.write && io.bus.cmd.valid,
      mask  = io.bus.cmd.mask
    )
    io.bus.cmd.ready := True
    
    val readData = 
    ram.readWriteSync(
      address = io.dpram.ADDR,
	  data    = io.dpram.WDATA,
	  enable  = True,
	  write   = io.dpram.WRITE && io.dpram.CS
    )

    io.dpram.RDATA	:= readData
  }
}


case class UserIntfConfig(
  addressWidth  : Int,
  dataWidth     : Int
)


object UserIntf{

  def apply(addressWidth: Int, dataWidth: Int) = new UserIntf(UserIntfConfig(addressWidth = addressWidth, dataWidth = dataWidth))
}


case class UserIntf(config: UserIntfConfig) extends Bundle with IMasterSlave {

  // outputs
  val ADDR    = UInt(config.addressWidth bits)
  val CS      = Bool()
  val READ    = Bool()
  val WRITE   = Bool()
  val WDATA   = Bits(config.dataWidth bits)
  
  // inputs
  val RDATA   = Bits(config.dataWidth bits)

  override def asMaster(): Unit = {
    out(ADDR, CS, READ, WRITE, WDATA)
    in(RDATA)
  }

  def << (sink: UserIntf): Unit = sink >> this

  def >> (sink: UserIntf): Unit = {
    sink.ADDR   := this.ADDR.resized
    sink.CS     := this.CS
    sink.READ   := this.READ
    sink.WRITE  := this.WRITE
    sink.WDATA  := this.WDATA
    this.RDATA  := sink.RDATA
  }
}


case class PipelinedMemoryBusUserInterface(userIntfConfig: UserIntfConfig, pipelineBridge : Boolean, pipelinedMemoryBusConfig : PipelinedMemoryBusConfig) extends Component{
  assert(userIntfConfig.dataWidth == pipelinedMemoryBusConfig.dataWidth)
  assert(userIntfConfig.addressWidth <= pipelinedMemoryBusConfig.addressWidth)

  val io = new Bundle {
    val pipelinedMemoryBus = slave(PipelinedMemoryBus(pipelinedMemoryBusConfig))
    val userIntf = master(UserIntf(userIntfConfig))
  }

  val pipelinedMemoryBusStage = PipelinedMemoryBus(pipelinedMemoryBusConfig)
  pipelinedMemoryBusStage.cmd << (if(pipelineBridge) io.pipelinedMemoryBus.cmd.halfPipe() else io.pipelinedMemoryBus.cmd)
  pipelinedMemoryBusStage.rsp >-> io.pipelinedMemoryBus.rsp

  val state = RegInit(False)
  pipelinedMemoryBusStage.cmd.ready := False

  io.userIntf.CS      := pipelinedMemoryBusStage.cmd.valid
  io.userIntf.READ    := !pipelinedMemoryBusStage.cmd.write && state
  io.userIntf.WRITE   := pipelinedMemoryBusStage.cmd.write && state
  io.userIntf.ADDR    := pipelinedMemoryBusStage.cmd.address.resized
  io.userIntf.WDATA   := pipelinedMemoryBusStage.cmd.data

  pipelinedMemoryBusStage.rsp.valid := False
  pipelinedMemoryBusStage.rsp.data  := io.userIntf.RDATA

  when(!state) {
    state := pipelinedMemoryBusStage.cmd.valid
  } otherwise {
    state := False
    pipelinedMemoryBusStage.rsp.valid := !pipelinedMemoryBusStage.cmd.write
    pipelinedMemoryBusStage.cmd.ready := True
  }
}
