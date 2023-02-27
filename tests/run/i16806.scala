//scalajs: --skip
import java.util.concurrent.Semaphore

object Repro {

  case object DFBit
  final class DFError extends Exception("")
  final class DFType[+T](val value: T | DFError) extends AnyVal

  def asIR(dfType: DFType[DFBit.type]): DFBit.type = dfType.value match
  case dfTypeIR: DFBit.type => dfTypeIR
  case err: DFError         => throw new DFError

  object Holder {
    val s = new Semaphore(1, false)
    final lazy val Bit = {
      s.release()
      new DFType[DFBit.type](DFBit)
    }
  }

  @main
  def Test =
    val a = new Thread() {
      override def run(): Unit =
        Holder.s.acquire()
        val x = Holder.Bit.value
        assert(x.isInstanceOf[DFBit.type])
        println("Success")
    }
    val b = new Thread() {
      override def run(): Unit =
        Holder.s.acquire()
        val x = Holder.Bit.value
        assert(x.isInstanceOf[DFBit.type])
        println("Success")
    }
    a.start()
    b.start()
    a.join(300)
    b.join(300)
}