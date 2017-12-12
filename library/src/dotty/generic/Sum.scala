package dotty.generic

/** Represents a value of several possible types. An instance of Sum is either
 *  an instance of SLeft or of SRight. Generalises Either to arbitrary arity.
 */
sealed trait Sum

/** Non-empty sum */
sealed trait SCons[H, T <: Sum] extends Sum

/** Left case of a sum */
final case class SLeft[H, T <: Sum](head: H) extends SCons[H, T]

/** Right case of a sum */
final case class SRight[H, T <: Sum](tail: T) extends SCons[H, T]

/** Empty sum */
sealed trait SNil extends Sum
