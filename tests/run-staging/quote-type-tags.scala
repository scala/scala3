import scala.quoted.*
import scala.quoted.staging.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    def asof[T: Type, U](x: Expr[T], t: Type[U]): Expr[U] =
      '{$x.asInstanceOf[t.Underlying]}

    println(asof('{}, Type.of[Unit]).show)
    println(asof('{true}, Type.of[Boolean]).show)
    println(asof('{0.toByte}, Type.of[Byte]).show)
    println(asof('{ 'a' }, Type.of[Char]).show)
    println(asof('{1.toShort}, Type.of[Short]).show)
    println(asof('{2}, Type.of[Int]).show)
    println(asof('{3L}, Type.of[Long]).show)
    println(asof('{4f}, Type.of[Float]).show)
    println(asof('{5d}, Type.of[Double]).show)

    println(asof('{5d}, Type.of[Boolean]).show) // Will clearly fail at runtime but the code can be generated
    '{}
  }
}
