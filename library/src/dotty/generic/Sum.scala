package dotty.generic

sealed trait Sum[X]
sealed trait SCons[H[_], T[t] <: Sum[t], X] extends Sum[X]
final case class SLeft[H[_], T[t] <: Sum[t], X](head: H[X]) extends SCons[H, T, X]
final case class SRight[H[_], T[t] <: Sum[t], X](tail: T[X]) extends SCons[H, T, X]
sealed trait SNil[X] extends Sum[X]
