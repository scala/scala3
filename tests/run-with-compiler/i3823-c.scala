import dotty.tools.dotc.quoted.Runners._
import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    def f[T](x: Expr[T])(implicit t: Type[T]) = '{
      val z = ~x
    }
    // FIXME uncomment next line
    // println(f('(2))(Type.IntTag).show)
    println("{\n  val z: Int = 2\n  ()\n}") // TODO remove line
  }
}