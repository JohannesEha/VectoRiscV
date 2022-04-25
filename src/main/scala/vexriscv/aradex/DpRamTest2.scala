package vexriscv.aradex

import spinal.core._
import spinal.lib._

case class DpRamTest2() extends Component{
  val ram = Mem(Bits(8 bits), 1024)

  val	 address_1 = in UInt(10 bits)
  val writeData_1 = in Bits(8 bits)
  val readData_1 = out Bits(8 bits)
  val enable_1 = True	//in Bool()
  val write_1 = in Bool()
  readData_1 := ram.readWriteSync(address_1, writeData_1, enable_1, write_1)

  val address_2 = in UInt(10 bits)
  val writeData_2 = in Bits(8 bits)
  val readData_2 = out Bits(8 bits)
  val enable_2 = True	//in Bool()
  val write_2 = in Bool()
  readData_2 := ram.readWriteSync(address_2, writeData_2, enable_2, write_2)
}


object DpRamTest2Vhdl{
  def main(args: Array[String]) {
    SpinalVhdl(DpRamTest2())
  }
}
