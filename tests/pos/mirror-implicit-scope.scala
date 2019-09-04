import scala.deriving._

object Test {
  object K0 {
    type Generic[T] = Mirror { type Scope = K0.type ; type MirroredType = T ; type MirroredElemTypes }
    given Ops {
      inline def (gen: Generic[T]) toRepr[T <: Product](t: T): gen.MirroredElemTypes = Tuple.fromProduct(t).asInstanceOf
    }
  }

  object K1 {
    type Generic[F[_]] = Mirror { type Scope = K1.type ; type MirroredType = F ; type MirroredElemTypes[_] }
    given Ops {
      inline def (gen: Generic[F]) toRepr[F[_] <: Product, T](t: F[T]): gen.MirroredElemTypes[T] = Tuple.fromProduct(t).asInstanceOf
    }
  }

  case class ISB(i: Int, s: String, b: Boolean)
  val v0 = the[K0.Generic[ISB]]
  val v1 = v0.toRepr(ISB(23, "foo", true))
  val v2: (Int, String, Boolean) = v1

  case class Box[T](t: T)
  val v3 = the[K1.Generic[Box]]
  val v4 = v3.toRepr(Box(23))
  val v5: Tuple1[Int] = v4
}
