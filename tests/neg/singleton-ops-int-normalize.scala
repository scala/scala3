import scala.compiletime.ops.int.*

object Test:
  type Pos <: Int
  type Neg <: Int

  // Non-singleton types are not grouped.
  summon[Pos - Pos + Neg =:= Neg] // error
  summon[Pos + Pos =:= 2 * Pos] // error
