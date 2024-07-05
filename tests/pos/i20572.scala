//> using options -Werror
trait Writes[T]
trait Format[T] extends Writes[T]
given [T: List]: Writes[T] = null
given [T]: Format[T] = null

val _ = summon[Writes[Int]]
