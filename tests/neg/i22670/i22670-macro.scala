// nopos-error
//package `X-Y` // explicit package name gets a diagnostic

import scala.quoted.*

transparent inline def foo =
  ${ fooImpl }

def fooImpl(using Quotes): Expr[Any] =
  Expr("hello")
