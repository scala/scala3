//> using scala "3.3.1"
//> using dep org.http4s::http4s-ember-client:1.0.0-M40
//> using dep org.http4s::http4s-ember-server:1.0.0-M40
//> using dep org.http4s::http4s-dsl:1.0.0-M40

//import cats.effect.*
//import cats.implicits.*

class Concurrent[F[_]]

class Test[F[_]: Concurren]: // error
    def hello = ???

object Test:
    def apply[F[_]: Concurrent] = new Test[F]
