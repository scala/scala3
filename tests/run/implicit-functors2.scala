object Utils {
  type Id[t] = t
  type Const[c] = [t] =>> c
}

import Utils._

abstract class ErasedInstances { type FT }
class ErasedProductInstances(override val toString: String) extends ErasedInstances
class ErasedCoproductInstances(override val toString: String) extends ErasedInstances

object K1 {
  type Instances[F[_[_]], T[_]] = ErasedInstances { type FT = F[T] ; type C = F }
}

class Functor[F[_]](override val toString: String)

object Functor {
  inline def apply[F[_]](implicit ff: Functor[F]): Functor[F] = ff

  implicit val functorId: Functor[Id] = new Functor("functorId")

  implicit def functorNested[F[_], G[_]](implicit ff: Functor[F], fg: Functor[G]): Functor[[t] =>> F[G[t]]] = new Functor(s"functorNested($ff, $fg)")

  implicit def functorGen[F[_]](implicit inst: K1.Instances[Functor, F]): Functor[F] = new Functor(s"functorGen($inst")

  implicit def functorConst[T]: Functor[Const[T]] = new Functor(s"functorConst")
}

sealed trait Opt[+A]
object Opt {
  implicit def optInstances[F[_[_]]](implicit fs: F[Sm], fn: F[[t] =>> Nn.type]): ErasedCoproductInstances { type FT = F[Opt] ; type C = F } =
    new ErasedCoproductInstances(s"optInstances($fs, $fn)") { type FT = F[Opt] ; type C = F }
}

case class Sm[+A](value: A) extends Opt[A]
object Sm {
  implicit def smInstances[F[_[_]]](implicit fi: F[Id]): ErasedProductInstances { type FT = F[Sm] ; type C = F } =
    new ErasedProductInstances(s"smInstances($fi)") { type FT = F[Sm] ; type C = F }
}

case object Nn extends Opt[Nothing]

object Test extends App {
  assert(Functor[Const[Nn.type]].toString == "functorConst")
  assert(Functor[Sm].toString == "functorGen(smInstances(functorId)")
  assert(Functor[Opt].toString == "functorGen(optInstances(functorGen(smInstances(functorId), functorConst)")
  assert(Functor[[t] =>> Opt[Opt[t]]].toString == "functorNested(functorGen(optInstances(functorGen(smInstances(functorId), functorConst), functorGen(optInstances(functorGen(smInstances(functorId), functorConst))")
}

