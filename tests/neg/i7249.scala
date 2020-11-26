import Predef.{$conforms => _, _}

trait F[H, T]


object Test extends App {
  given [H, T] => (h: H, t: T) => F[H, T] as f = ???
  summon[F[Int, Unit]]  // error
}