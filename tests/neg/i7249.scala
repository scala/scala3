import Predef.{$conforms => _, _}

trait F[H, T]


object Test extends App {
  given f[H, T](given h: H, t: T): F[H, T] = ???
  summon[F[Int, Unit]]  // error
}