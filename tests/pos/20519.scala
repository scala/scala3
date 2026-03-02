class Box[T](val value: T)

def boo[F[_], A](e: F[Box[A]]): F[A] = ???

type Result[G[_], B] = G[Box[B]]

def main =
  val b: Result[Option, Int] = ???
  val c = boo(b)
  c: Option[Int]
