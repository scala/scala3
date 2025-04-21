//> using options -Werror -Wunused:imports

import scala.reflect.TypeTest

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
