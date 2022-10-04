import scala.compiletime.ops.int.*

object Test:
  type ToInt[X] <: Singleton & Int

  // Insertion order is preserved for non-singleton types.
  summon[ToInt[{val a: 2}] + ToInt[{val b: 2}] =:= ToInt[{val b: 2}] + ToInt[{val a: 2}]] // error
