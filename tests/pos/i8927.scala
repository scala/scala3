import scala.language.implicitConversions

trait Eq[k <: AnyKind, K[_ <: k]]:
  extension [A <: k, B <: k](k: K[A]) def isEq (k2: K[B]): Eq.GEQ[k, K, A, B]

object Eq:
  enum GEQ[k <: AnyKind, K[_ <: k], A <: k, B <: k]:
    case Y[k <: AnyKind, K[_ <: k], A <: k](res: K[A]) extends GEQ[k, K, A, A]
    case N()

sealed trait DPair[k <: AnyKind, K[_ <: k], +V[_ <: k]]:
  type A <: k
  val key: K[A]
  val value: V[A]
  final def extract[B <: k](k: K[B])(using Eq[k, K]): Option[V[B]] = key isEq k match
    case y: Eq.GEQ.Y[k, K, A] => Some(value)
    case _    => None

object DPair:
  given pair: [k, K[_ <: k], V[_ <: k], C <: k] => Conversion[(K[C], V[C]), DPair[k, K, V]] = tup =>
    case class dpair(key: K[C], value: V[C]) extends DPair[k, K, V]:
      type A = C
    dpair(tup._1, tup._2)
