sealed abstract class Free[S[_], A] {
  final def map[B](f: A => B): Free[S, B] = ???
  final def flatMap[B](f: A => Free[S, B]): Free[S, B] = new Free[S, B] {}
}

trait Parameter[T]
def namedDouble(name: String): Free[Parameter, Double] = ???

type Double2 = (Double, Double)
type Double3 = (Double, Double, Double)
val spec: Free[Parameter, Either[Double3, Double2]] = for {
  result <-
    if (???) {
      for {
        x <- namedDouble("X")
        y <- namedDouble("Y")
        z <- namedDouble("Z")
      } yield Left((x, y, z))
    } else {
      for {
        x <- namedDouble("X")
        y <- namedDouble("Y")
      } yield Right((x, y))
    }
} yield result