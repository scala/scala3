import scala.compiletime.ops.int.*

object Test:
  type ToInt[X] <: Singleton & Int

  // Insertion order should be preserved for all types for which an order
  // has not (yet) been defined. Refinements are such an example.
  summon[ToInt[{val a: 2}] + ToInt[{val b: 2}] =:= ToInt[{val b: 2}] + ToInt[{val a: 2}]] // error

  // Operations containing non-singleton arguments are left as-is.
  val m: Long = 2L
  summon[(3 | 2) + (3 | 2) =:= 2 * (3 | 2)] // error
  summon[(3L | 2L) + (3L | 2L) + m.type + m.type =:= 2L * m.type + (3L | 2L) + (3L | 2L)] // error
  summon[(3L | 2L) - (3L | 2L) =:= 0] // error
  summon[Int - Int =:= 0] // error
