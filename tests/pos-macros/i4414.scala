import scala.quoted._

class Test {

  def a[A](using s: Scope)()(using s.Type[A]): Unit = {
    def f(using s2: Scope)(using s2.Type[s.Expr[A]]): Unit = b[s.Expr[A]]()
    a[A]()
  }

  def b[A](using s: Scope)()(using s.Type[A]): Unit = ???
}
