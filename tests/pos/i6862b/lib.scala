
trait Expr[+T]

trait Ctx

inline def foo(): Int = splice( bar() )
def bar() with Ctx : Expr[Int] = ???
def splice[T](f: Ctx ?=> Expr[T]): T = ???
