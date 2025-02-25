//> using options -Werror -Wunused:imports

import scala.reflect.{Typeable, TypeTest}
import compiletime.*

trait A {
  type B
  type C <: B

  given instance: TypeTest[B, C]
}

def f(a: A, b: a.B): Boolean = {
  import a.C
  b match {
    case _: C =>
      true
    case _ =>
      false
  }
}

trait T:
  type X
  given Typeable[X] = deferred

def g(t: T, x: Any) =
  import t.X
  x match
  case _: X => true
  case _ => false

def typer[T: Typeable](x: Any) =
  x match
  case _: T => 1
  case _ => 0
