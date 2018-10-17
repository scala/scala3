import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)
    def asof[T: Type, U](x: Expr[T], t: Type[U]): Expr[U] =
      '{$x.asInstanceOf[$t]}

    def asof[T, U](x: Expr[T], t: Type[U]): Staged[U] =
      '{$x.asInstanceOf[$t]}

    println(tb.show(asof('{}, '[Unit])))
    println(tb.show(asof('{true}, '[Boolean])))
    println(tb.show(asof('{0.toByte}, '[Byte])))
    println(tb.show(asof('{ 'a' }, '[Char])))
    println(tb.show(asof('{1.toShort}, '[Short])))
    println(tb.show(asof('{2}, '[Int])))
    println(tb.show(asof('{3L}, '[Long])))
    println(tb.show(asof('{4f}, '[Float])))
    println(tb.show(asof('{5d}, '[Double])))

    println(tb.show(asof('{5d}, '[Boolean]))) // Will clearly fail at runtime but the code can be generated
  }
}
