import Predef.{$conforms => _, _}

trait F[H, T]


object Test extends App {
  given f[H, T] with (h: H, t: T) as F[H, T] = ???
  summon[F[Int, Unit]]  // error
}