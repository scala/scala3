package scala

import annotation.experimental

@experimental
object NamedTuple:

  type DropNames[T <: Tuple] = T match
    case Tuple.NamedValue[_, x] *: xs => x *: DropNames[xs]
    case _ => T

  extension [T <: Tuple](x: T) def dropNames: DropNames[T] =
    x.asInstanceOf // named and unnamed tuples have the same runtime representation
