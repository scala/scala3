package dotty.generic

/** Represents a heterogeneous collection: a datatype capable of storing data
 *  of different types. Generalises Tuples to arbitrary arity.
 */
sealed trait Prod

/** Non-empty product */
final case class PCons[H, T <: Prod](head: H, tail: T) extends Prod

/** Empty product */
final case class PNil() extends Prod
