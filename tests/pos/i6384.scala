trait Tc1[A]
trait Tc2[A]

class X
object X {
  implicit def catchAll[F[_]]: F[X] = ???
}

type ManualLambda[a] = Tc1[a] & Tc2[a]

object app extends App {
  implicitly[Tc1[X]] //ok
  implicitly[ManualLambda[X]] // ok
  implicitly[Tc1[X] & Tc2[X]] // no implicit argument of type Tc1[X] & Tc2[X] was found for parameter ev of method implicitly in object Predef
}
