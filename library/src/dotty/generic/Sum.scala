package dotty.generic

sealed trait Sum
sealed trait SCons[H, T <: Sum] extends Sum
final case class SLeft[H, T <: Sum](head: H) extends SCons[H, T]
final case class SRight[H, T <: Sum](tail: T) extends SCons[H, T]
sealed trait SNil extends Sum
