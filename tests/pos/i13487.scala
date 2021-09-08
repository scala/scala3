trait TC[F[_[_], _]]
object TC {
  def derived[F[_[_], _]]: TC[F] = ???
}

case class Foo[A](a: A) derives TC
