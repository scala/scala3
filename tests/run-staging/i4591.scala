import scala.quoted._
import scala.quoted.staging._

object Test {

  def foo[T](using s: Scope)(init: s.Expr[T])(using s.Type[T]): s.Expr[Unit] = '{
    var x = $init
    println(x)
  }

  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    run(foo('{Option(9)}))
  }

}
