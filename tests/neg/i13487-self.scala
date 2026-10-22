//> using options -language:experimental.modularity

// Self-based counterpart of neg/i13487.scala

trait TC:
  type Self[_, _[_]]
object TC {
  def derived[F[_, _[_]]]: F is TC = ???
}

case class Foo[A](a: A) derives TC // error
