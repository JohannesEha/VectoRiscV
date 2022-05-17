package vexriscv.aradex

import spinal.core._
import spinal.lib._
import vexriscv.{VexRiscv, _}
import vexriscv.plugin._

object IntSqrtPlugin{
  object IS_INT_SQRT extends Stageable(Bool)
}

class IntSqrtPlugin extends Plugin[VexRiscv] {
  import IntSqrtPlugin._

  override def setup(pipeline: VexRiscv): Unit = {
    import pipeline.config._

    val actions = List[(Stageable[_ <: BaseType],Any)](
      SRC1_CTRL                 -> Src1CtrlEnum.RS,
      REGFILE_WRITE_VALID       -> True,
      BYPASSABLE_EXECUTE_STAGE  -> Bool(pipeline.stages.last == pipeline.execute),
      BYPASSABLE_MEMORY_STAGE   -> True,
      RS1_USE                   -> True,
      IS_INT_SQRT               -> True
    )

    val decoderService = pipeline.service(classOf[DecoderService])

    decoderService.addDefault(IS_INT_SQRT, False)
    
    val key = M"000001100000-----000-----0110011"
    decoderService.add(key, actions)
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline._
    import pipeline.config._
   
    memory plug new Area {
      import memory._
      def area = this

      val counter = Counter(19)
      val done = Reg(Bool) setWhen(counter === counter.end-1) clearWhen(!arbitration.isStuck)
      val result = Reg(Bits(32 bits))

      //register allocation
      val x = Reg(UInt(32 bits))  // input radicand we want the square root of
      val a = Reg(UInt(32 bits))  // holds the current value we’re working on
      val q = Reg(UInt(16 bits))  // the square root

      //Execute stage logic to drive memory stage's input regs
      when(!arbitration.isStuck){
        x := execute.input(RS1).asUInt
        a := U(0)
        q := U(0)
        counter.clear()
      }

      when(arbitration.isValid && input(IS_INT_SQRT)){
        when(!done){
          arbitration.haltItself := True
          counter.increment()

          val iterationArea = new Area {
            val sel = CombInit(a)
            val t = a-(q @@ U"01")    // result of sign test

            when(!t.msb){
              sel := t.resized
            }

            a := (sel @@ x(widthOf(x)-2,2 bits)).resized
            x := x |<< 2
            q := (q @@ !t.msb).resized
          }.setCompositeName(area, "iteration")

          when(counter === counter.end-1){
            result := q.asBits.resized
          }
        }

        output(REGFILE_WRITE_DATA) := result
      }
    }
  }
}
