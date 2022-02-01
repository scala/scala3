import scala.compiletime.ops.long.*

object Test:
  type ToInt[X] <: Singleton & Long

  // Insertion order should be preserved for all types for which an order
  // has not (yet) been defined. Refinements are such an example.
  summon[ToInt[{val a: 2L}] + ToInt[{val b: 2L}] =:= ToInt[{val b: 2L}] + ToInt[{val a: 2L}]] // error

  // Operations containing non-singleton arguments are left as-is.
  summon[(3L | 2L) + (3L | 2L) =:= 2L * (3L | 2L)] // error
  summon[(3L | 2L) - (3L | 2L) =:= 0] // error
  summon[Int - Int =:= 0] // error

  // No implicit conversions from Ints to Longs
  summon[1L + 2 =:= 1 + 2L] // error
  summon[1 + 2L =:= 3L] // error
  val m: Long = 42
  summon[m.type + 1 =:= m.type + 1L] // error
