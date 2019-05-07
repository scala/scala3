trait Tc1[A]
trait Tc2[A] extends Tc1[A]

class PinTypeTo[K[_]]
object PinTypeTo {
  implicit val pinType: PinTypeTo[Tc2] = new PinTypeTo[Tc2]
}

class X
object X {
  implicit def Tc2Instance[F[x] >: Tc2[x]: PinTypeTo]: F[X] = new Tc2[X] {}
}

object app extends App {
  implicitly[Tc2[X]]
  implicitly[Tc1[X]]
}