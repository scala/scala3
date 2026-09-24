//> using options -Werror
trait Writes[T]
trait Format[T] extends Writes[T]
given [T: List] => Writes[T] = ???
given [T] => Format[T] = ???

val _ = summon[Writes[Int]]
