trait TC[X]
object TC {
  given [T, S <: TC[S]](using TC[S]): TC[T] = ???
  summon[TC[Int]] // error
}
