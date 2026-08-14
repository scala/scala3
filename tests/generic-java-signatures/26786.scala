import compiletime.ops.int.>

trait Check[Cond[T <: Int] <: Boolean]:
  def apply(arg: Int): Unit

type CondT = [t <: Int] =>> t > 0

class X:
  val foo: Check[CondT] = new Check[CondT]:
    def apply(arg: Int): Unit = ()

object Test:
  def main(args: Array[String]): Unit =
    classOf[X].getMethods.filter(_.getName.contains("foo")).sortBy(_.getName).foreach(m => println(m.toGenericString))
