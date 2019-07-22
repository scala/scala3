
trait Expr[+T]

trait Ctx

inline def foo(): Int = splice( bar() )
def bar() given Ctx: Expr[Int] = ???
def splice[T](f: given Ctx => Expr[T]): T = ???
