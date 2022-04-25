package vexriscv.aradex

import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple._

case class DpRamTest() extends Component{

  val pipelinedMemoryBusConfig = PipelinedMemoryBusConfig(
    addressWidth = 32,
    dataWidth = 32
  )

  val dpRamSize : BigInt = 8 kB

  val dpramIntfConfig = DpramIntfConfig(
    addressWidth = 11,
    dataWidth = 32
  )

  val io = new Bundle{
    val bus = slave(PipelinedMemoryBus(pipelinedMemoryBusConfig))
	val dpram = slave(DpramInterface(dpramIntfConfig))
  }

  val ram = Mem(Bits(32 bits), dpRamSize / 4)
  ram.addAttribute("ramstyle", "no_rw_check")

  io.bus.rsp.valid := RegNext(io.bus.cmd.fire && !io.bus.cmd.write) init(False)
  io.bus.rsp.data := ram.readWriteSync(
    address = (io.bus.cmd.address >> 2).resized,
    data  = io.bus.cmd.data,
    enable  = io.bus.cmd.valid,
    write  = io.bus.cmd.write,
    mask  = io.bus.cmd.mask
  )
  io.bus.cmd.ready := True
  
  val readData = 
  ram.readWriteSync(
    address = io.dpram.ADDR,
    data    = io.dpram.WDATA,
    enable  = io.dpram.CS,
    write   = io.dpram.WRITE
  )

  io.dpram.RDATA	:= readData
}


object DpRamTestVhdl{
  def main(args: Array[String]) {
    SpinalVhdl(DpRamTest())
  }
}
