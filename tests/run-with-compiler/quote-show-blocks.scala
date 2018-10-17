import scala.quoted._
import scala.quoted.autolift._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)
    def a(n: Int, x: Expr[Unit]): Staged[Unit] =
      if (n == 0) x
      else a(n - 1, '{ println(${n}); $x })

    println(tb.show(a(5, '{})))


    def b(n: Int, x: Expr[Unit]): Staged[Unit] =
      if (n == 0) x
      else b(n - 1, '{ $x; println(${n}) })

    println(tb.show(b(5, '{})))
  }

}
