import scala.quoted._
import scala.quoted.staging._

object Test {

  inline def foo(using s: Scope)(x: s.Expr[Int]): s.Expr[Int] = '{
    println("foo")
    $x
  }

  implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = usingNewScope {
    val y = '{45}
    println(foo(y).show)
  }

}
