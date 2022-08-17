// derived from shapeless 2 tests at https://github.com/milessabin/shapeless/blob/ae9fc166082adb3fa590a604b256c9e87a467c56/core/src/test/scala/shapeless/generic.scala

import shapeless.impl.*

object PathVariantDefns {
  sealed trait AtomBase {
    sealed trait Atom
    case class Zero(value: String) extends Atom
  }

  trait Atom1 extends AtomBase {
    case class One(value: String) extends Atom
  }

  trait Atom2 extends AtomBase {
    case class Two(value: String) extends Atom
  }

  object Atoms01 extends AtomBase with Atom1
  object Atoms02 extends AtomBase with Atom2
}

object PathVariants {
  import PathVariantDefns._

  val gen1 = Generic[Atoms01.Atom] // error: we do not support this yet
  implicitly[gen1.Repr =:= (Atoms01.One :+: Atoms01.Zero :+: CNil)] // error: knock on

  val gen2 = Generic[Atoms02.Atom] // error: we do not support this yet
  implicitly[gen2.Repr =:= (Atoms02.Two :+: Atoms02.Zero :+: CNil)] // error: knock on
}


////// ADAPTER CODE ////////

package shapeless {
  object impl {
    import scala.deriving.Mirror

    def assertEquals(a: Any, b: Any): Unit =
      assert(a == b, s"$a != $b")

    def assertTypedEquals[T](a: T, b: T): Unit =
      assertEquals(a, b)

    def typed[T](expr: T): Unit = ()

    type HList = Tuple
    type HNil = EmptyTuple
    type ::[+H, +T <: HList] = *:[H, T]

    case class CList(e: Any)
    class CNil private () extends CList(EmptyTuple)
    class :+:[+H, +T <: CList](elem: Any) extends CList(elem)

    def Inl[E](elem: E): E :+: Nothing = :+:(elem)
    def Inr[Elems <: CList](elems: Elems): Nothing :+: Elems = :+:(elems.e)

    type CListRefl[T <: Tuple] = T match {
      case EmptyTuple => CNil
      case h *: tl => h :+: CListRefl[tl]
    }

    extension [T](gen: Generic[T])(using m: Mirror.Of[T])
      transparent inline def to(t: T) =
        inline compiletime.erasedValue[m.type] match
          case _: Mirror.ProductOf[T] =>
            Tuple.fromProduct(t.asInstanceOf[Product]).asInstanceOf[m.MirroredElemTypes]
          case _: Mirror.SumOf[T] =>
            Inl(t).asInstanceOf[CListRefl[m.MirroredElemTypes]]

      transparent inline def from[R](r: R) =
        inline compiletime.erasedValue[m.type] match
          case _: Mirror.ProductOf[T] =>
            m.asInstanceOf[Mirror.ProductOf[T]].fromProduct(r.asInstanceOf[Product]).asInstanceOf[T]
          case _: Mirror.SumOf[T] =>
            r.asInstanceOf[CList].e.asInstanceOf[T]

    trait Generic[T]:
      type Repr

    object Generic:

      type Repr[T, M, Elems] = M match
        case Mirror.Sum { type MirroredType = T } => CListRefl[Elems]
        case Mirror.Product { type MirroredType = T } => Elems

      transparent inline given [T](using m: Mirror.Of[T]): Generic[T] = apply[T]

      transparent inline def apply[T](using m: Mirror.Of[T]) = new Generic[T] {
        type Repr = Generic.Repr[T, m.type, m.MirroredElemTypes]
      }

      transparent inline def materialize[T, R](using m: Mirror.Of[T])(using
        R <:< Repr[T, m.type, m.MirroredElemTypes]): Unit = ()
  }
}
