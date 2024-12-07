//> using options -Ykind-projector:underscores

import scala.quoted.Type
import scala.quoted.Quotes

def x[A](t: Type[A])(using Quotes): Boolean = t match
  case '[_ *: _] =>
    true
  case _ =>
    false
