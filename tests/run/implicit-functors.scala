object Utils {
  type Id[t] = t
  type Const[c] = [t] =>> c
}

import Utils._

class Instances[F[_[_]], T[_]]

class Functor[F[_]](val kind: String)

object Functor {
  implicit val functorId: Functor[Id] = new Functor[Id]("id")
  implicit def functorGen[F[_]](implicit inst: Instances[Functor, F]): Functor[F] = new Functor[F]("gen")
  implicit def functorConst[T]: Functor[Const[T]] = new Functor[Const[T]]("const")
}

case class Sm[A](value: A)
object Sm {
  implicit def smInstances[F[_[_]]]: Instances[F, Sm] = null
}

object Test extends App {
  assert(implicitly[Functor[Sm]].kind == "gen")
}