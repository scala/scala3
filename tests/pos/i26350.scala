// https://github.com/scala/scala3/issues/26350
// A match-type case body that fails to constant-fold must not error at
// definition when that branch is never selected.
import scala.compiletime.ops.int.*

// Never applied; the `case _ => 1 / 0` branch must not be evaluated.
type AnIntA[T] <: Int = T match
  case Int => Int
  case _   => 1 / 0

// Explicit bound; using it with a scrutinee that selects `case Int` must work.
type AnIntB[T] <: Int = T match
  case Int => T & Int
  case _   => 1 / 0

val x: AnIntB[Int] = 5
val _ = summon[AnIntB[Int] =:= Int]
