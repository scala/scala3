import scala.quoted._
import scala.quoted.staging._

object Test:

  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit =
    def reduce[T](using s: Scope)(succ: (s: Scope) ?=> s.Expr[T] => s.Expr[T], zero: s.Expr[T])(using s.Type[T]): s.Expr[T] = '{
      var z = $zero
      ${ succ('z) }
    }
    def resCode2(using s: Scope): s.Expr[Int] =
      reduce[Int](x => '{$x + 1}, '{0})

    println(usingNewScope(resCode2.show))
