import scala.quoted._

inline def foo = ${fooImpl}

def fooImpl(using s: Scope) = {
  import s.tasty._
  val res = Expr.ofList(List('{"One"}))
  Expr(res.show)
}
