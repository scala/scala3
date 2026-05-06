//> using options -Werror -Wconf:id=E222:e -explain

//package `X-Y` // explicit package name gets a diagnostic
package xy // error named package required for warning

import scala.quoted.*

transparent inline def foo =
  ${ fooImpl }

def fooImpl(using Quotes): Expr[Any] =
  Expr("hello")
