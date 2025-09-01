trait TCl[F[_]]

def boo[F[_], A](e: F[Option[A]], ev: TCl[F]): Unit = ()

type Result[F[_], A] = F[Option[A]]

@main def main =
  summon[Result[Option, Int] =:= Option[Option[Int]]]

  val ev = new TCl[Option] {}

  val b: Result[Option, Int] = None
  boo(b, ev)

  val b2: Option[Option[Int]] = None
  boo(b2, ev)
