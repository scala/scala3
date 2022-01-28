import scala.compiletime.ops.int.*

object Test:
  type ToInt[X] <: Singleton & Int

  // Insertion order should be preserved for all types for which an order
  // has not (yet) been defined. Refinements are such an example.
  summon[ToInt[{val a: 2}] + ToInt[{val b: 2}] =:= ToInt[{val b: 2}] + ToInt[{val a: 2}]] // error

  // Non-singleton arguments are left as-is.
  summon[(3 | 2) + (3 | 2) =:= 2 * (3 | 2)] // error
