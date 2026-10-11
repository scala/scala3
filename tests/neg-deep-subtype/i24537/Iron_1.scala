package io.github.iltotore.iron

import scala.language.implicitConversions

opaque type IronType[A, C] <: A = A
type :|[A, C] = IronType[A, C]
object IronType:
  inline def apply[A, C](value: A): IronType[A, C] = value

final class Implication[C1, C2]
type ==>[C1, C2] = Implication[C1, C2]
object Implication:
  given [C]: (C ==> C) = Implication()
  given [C1, C2](using C1 <:< C2): (C1 ==> C2) = Implication()

implicit inline def autoCastIron[A, C1, C2](inline value: A :| C1)(using C1 ==> C2): A :| C2 =
  value.asInstanceOf

implicit inline def autoFactorize[A, I[_] <: Iterable[?], C1, C2](
    inline iterable: I[A :| C1]
)(using C1 ==> C2): I[A] :| ForAll[C2] = iterable.asInstanceOf

final class ForAll[C]
