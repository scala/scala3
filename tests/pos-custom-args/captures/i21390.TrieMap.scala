// Minimised from scala.collection.concurrent.LNode
// Useful as a minimisation of how,
// If we were to change the type interpolation
// to minimise to the inferred "X" type,
// then this is a minimisation of how the (ab)use of
// GADT constraints to handle class type params
// can fail PostTyper, -Ytest-pickler, and probably others.

import scala.language.experimental.captureChecking

class Foo[X](xs: List[X]):
  def this(a: X, b: X) = this(if (a == b) then a :: Nil else a :: b :: Nil)
