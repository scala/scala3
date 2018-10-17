import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    def f[T](x: Expr[T])(implicit t: Type[T]): Staged[Unit] = '{
      val z = $x
    }
    val tb = Toolbox.make
    println(tb.show(f('(2))(Type.IntTag)))
  }
}
