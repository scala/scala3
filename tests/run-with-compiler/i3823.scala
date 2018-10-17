import scala.quoted._

object Test {
  val tb = Toolbox.make
  def main(args: Array[String]): Unit = {
    def f[T: Type](x: Expr[T])(t: Type[T]): Staged[Unit] = '{
      val z: t.unary_~ = ~x
    }
    println(tb.show(f('(2))('[Int])))
  }
}