import scala.deriving.*

object Test {
  object K0 {
    type Generic[T] = Mirror { type Scope = K0.type ; type MirroredType = T ; type MirroredElemTypes }
    extension [T <: Product](gen: Generic[T]) {
      inline def toRepr (t: T): gen.MirroredElemTypes = Tuple.fromProduct(t).asInstanceOf
    }
  }

  object K1 {
    type Generic[F[_]] = Mirror { type Scope = K1.type ; type MirroredType = [X] =>> F[X] ; type MirroredElemTypes[_] }
    extension [F[_] <: Product, T](gen: Generic[F]) {
      inline def toRepr (t: F[T]): gen.MirroredElemTypes[T] = Tuple.fromProduct(t).asInstanceOf
    }
  }

  case class ISB(i: Int, s: String, b: Boolean)
  val v0 = summon[K0.Generic[ISB]]
  val v1 = v0.toRepr(ISB(23, "foo", true))
  val v2: (Int, String, Boolean) = v1

  case class Box[T](t: T)
  val v3 = summon[K1.Generic[Box]]
  val v4 = v3.toRepr(Box(23))
  val v5: Tuple1[Int] = v4
}
