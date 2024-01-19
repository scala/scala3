@annotation.implicitNotFound("there is no Inst!")
trait Inst

transparent inline def lookup[T] = compiletime.summonFrom[T] {
  case m: T => m
}
// given Inst = {}
val x = lookup[Inst] // error
