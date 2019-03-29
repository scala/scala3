object Utils {
  type Id[t] = t
  type Const[c] = [t] => c
}

import Utils._

class Instances[F[_[_]], T[_]]

trait Functor[F[_]]

object Functor {
  implicit val functorId: Functor[Id] = { println("functorId"); null }
  implicit def functorGen[F[_]](implicit inst: Instances[Functor, F]): Functor[F] = { println("funcorGen"); null }
  implicit def functorConst[T]: Functor[Const[T]] =  { println("funcorConst"); null }
}

case class Sm[A](value: A)
object Sm {
  implicit def smInstances[F[_[_]]]: Instances[F, Sm] = { println("smInstances"); null }
}

object Test extends App {
  implicitly[Functor[Sm]]
}