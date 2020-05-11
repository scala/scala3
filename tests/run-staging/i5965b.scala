import scala.quoted._
import scala.quoted.staging._

object Test {

  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)

    usingNewScope('[List])
    def list(using s: Scope) = bound('{List(1, 2, 3)})
    println(usingNewScope(list.show))
    println(run(list))

    def opt(using s: Scope) = bound('{Option(4)})
    println(usingNewScope(opt.show))
    println(run(opt))

    def map(using s: Scope) = bound('{Map(4 -> 1)})
    println(usingNewScope(map.show))
    println(run(map))
  }

  def bound[T, S[_]](using s: Scope)(x: s.Expr[S[T]])(using s.Type[T], s.Type[S]): s.Expr[S[T]] = '{
    val y = $x
    y
  }
}
