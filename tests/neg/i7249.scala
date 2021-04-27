import Predef.{$conforms as _, *}

trait F[H, T]


object Test extends App {
  given f[H, T](using h: H, t: T): F[H, T] = ???
  summon[F[Int, Unit]]  // error
}