case class Person(id: Int)

class GeodeContinuousSourceSpec {
  summon[PdxEncoder[Person]]
}

trait PdxEncoder[A] {
  def encode(a: A): Boolean
}

object PdxEncoder extends ObjectEncoder {
  implicit def intEncoder: PdxEncoder[Int] = ???
}

trait ObjectEncoder {
  given emptyTupleEncoder: PdxEncoder[EmptyTuple] = ???

  given tupleEncoder[K <: String, H, T <: Tuple](using
      m: ValueOf[K],
      hEncoder: PdxEncoder[H],
      tEncoder: PdxEncoder[T]
  ): PdxEncoder[FieldType[K, H] *: T] = ???

  given objectEncoder[A, Repr <: Tuple](using
      gen: LabelledGeneric.Aux[A, Repr],
      tupleEncoder: PdxEncoder[Repr]
  ): PdxEncoder[A] = ???
}

import scala.deriving.Mirror

private type FieldType[K, +V] = V & KeyTag[K, V]
private type KeyTag[K, +V]
private type ZipWith[T1 <: Tuple, T2 <: Tuple, F[_, _]] <: Tuple = (T1, T2) match {
  case (h1 *: t1, h2 *: t2) => F[h1, h2] *: ZipWith[t1, t2, F]
  case (EmptyTuple, ?)      => EmptyTuple
  case (?, EmptyTuple)      => EmptyTuple
  case _                    => Tuple
}

private trait LabelledGeneric[A] {
  type Repr
}

private object LabelledGeneric {
  type Aux[A, R] = LabelledGeneric[A] { type Repr = R }

  transparent inline given productInst[A <: Product](using
      m: Mirror.ProductOf[A]
  ): LabelledGeneric.Aux[A, ZipWith[m.MirroredElemLabels, m.MirroredElemTypes, FieldType]] =
    new LabelledGeneric[A] {
      type Repr = Tuple & ZipWith[m.MirroredElemLabels, m.MirroredElemTypes, FieldType]
    }
}