import scala.deriving.Mirror as M
import scala.deriving.*
import scala.Tuple.*
import scala.compiletime.*
import scala.compiletime.ops.int.S

trait Migration[-From, +To]:
  def apply(x: From): To

object Migration:

  extension [From](x: From)
    def migrateTo[To](using m: Migration[From, To]): To = m(x)

  given[T]: Migration[T, T] with
    override def apply(x: T): T = x

  type IndexOf[Elems <: Tuple, X] <: Int = Elems match {
    case (X *: elems) => 0
    case (_ *: elems) => S[IndexOf[elems, X]]
    case EmptyTuple => Nothing
  }

  inline def migrateElem[F,T, ToIdx <: Int](from: M.ProductOf[F], to: M.ProductOf[T])(x: Product): Any =

    type Label = Elem[to.MirroredElemLabels, ToIdx]
    type FromIdx = IndexOf[from.MirroredElemLabels, Label]
    inline constValueOpt[FromIdx] match

      case Some(fromIdx) =>
        type FromType = Elem[from.MirroredElemTypes, FromIdx]
        type ToType = Elem[to.MirroredElemTypes, ToIdx]
        summonFrom { case _: Migration[FromType, ToType] =>
          x.productElement(fromIdx).asInstanceOf[FromType].migrateTo[ToType]
        }

      case None =>
        type HasDefault = Elem[to.MirroredElemHasDefaults, ToIdx]
        inline erasedValue[HasDefault] match
          case _: true => to.defaultArgument(constValue[ToIdx])
          case _: false => compiletime.error("An element has no equivalent or default")


  inline def migrateElems[F,T, ToIdx <: Int](from: M.ProductOf[F], to: M.ProductOf[T])(x: Product): Seq[Any] =
    inline erasedValue[ToIdx] match
      case _: Tuple.Size[to.MirroredElemLabels] => Seq()
      case _ => migrateElem[F,T,ToIdx](from, to)(x) +: migrateElems[F,T,S[ToIdx]](from, to)(x)

  inline def migrateProduct[F,T](from: M.ProductOf[F], to: M.ProductOf[T])
                                (x: Product): T =
    val elems = migrateElems[F, T, 0](from, to)(x)
    to.fromProduct(new Product:
      def canEqual(that: Any): Boolean = false
      def productArity: Int = elems.length
      def productElement(n: Int): Any = elems(n)
    )

  inline def migration[F,T](using from: M.Of[F], to: M.Of[T]): Migration[F,T] = (x: F) =>
    inline from match
      case fromP: M.ProductOf[F] => inline to match
        case toP: M.ProductOf[T] => migrateProduct[F, T](fromP, toP)(x.asInstanceOf[Product])
        case _: M.SumOf[T] => compiletime.error("Cannot migrate sums")
      case _: M.SumOf[F] => compiletime.error("Cannot migrate sums")

end Migration


import Migration.*
object Test extends App:

  case class A1(x: Int)
  case class A2(x: Int)
  given Migration[A1, A2] = migration
  assert(A1(2).migrateTo[A2] == A2(2))

  case class B1(x: Int, y: String)
  case class B2(y: String, x: Int)
  given Migration[B1, B2] = migration
  assert(B1(5, "hi").migrateTo[B2] == B2("hi", 5))

  case class C1(x: A1)
  case class C2(x: A2)
  given Migration[C1, C2] = migration
  assert(C1(A1(0)).migrateTo[C2] == C2(A2(0)))

  case class D1(x: Double)
  case class D2(b: Boolean = true, x: Double)
  given Migration[D1, D2] = migration
  assert(D1(9).migrateTo[D2] == D2(true, 9))

  case class E1(x: D1, y: D1)
  case class E2(y: D2, s: String = "hi", x: D2)
  given Migration[E1, E2] = migration
  assert(E1(D1(1), D1(2)).migrateTo[E2] == E2(D2(true, 2), "hi", D2(true, 1)))

  // should only use default when needed
  case class F1(x: Int)
  case class F2(x: Int = 3)
  given Migration[F1, F2] = migration
  assert(F1(7).migrateTo[F2] == F2(7))

