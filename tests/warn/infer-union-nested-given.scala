//> using options -Winfer-union

trait Cmp[A, B]
object Cmp:
  given sub[A, B](using A <:< B): Cmp[A, B] = new Cmp[A, B] {}

trait Show[A]
object Show:
  given any[A]: Show[A] = new Show[A] {}

def eq[A, B](a: A, b: B)(using Cmp[A, B]): Unit = ()
def show[A](a: A)(using Show[A]): Unit = ()
def same[A](a: A, b: A): Unit = ()

def explicitArgs(x: Int | String) = eq[Int | String, Int | String](x, x) // ok
def summoned = summon[Cmp[Int | String, Int | String]] // ok
def noNestedUsing(x: Int | String) = show[Int | String](x) // ok
def inferred = same(1, "") // warn
