package test.shapeless {

import scala.collection.mutable.WrappedArray
import scala.compiletime._
import scala.deriving._
import annotation.tailrec

object K0 {
  type Generic[O] = Mirror { type MirroredType = O ; type MirroredElemTypes }
  type ProductGeneric[O] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes }

  def Generic[O] given (gen: Generic[O]): Generic[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen
  def ProductGeneric[O] given (gen: ProductGeneric[O]): ProductGeneric[O] { type MirroredElemTypes = gen.MirroredElemTypes ; type MirroredLabel = gen.MirroredLabel ; type MirroredElemLabels = gen.MirroredElemLabels } = gen

  opaque type Instances[F[_], T] = ErasedInstances[F[T]]
  opaque type ProductInstances[F[_], T] = ErasedProductInstances[F[T]]

  def Instances[F[_], T] given (inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_], T] given (inst: ProductInstances[F, T]): inst.type = inst

  type ToUnion[T] = T match {
    case Unit => Nothing
    case a *: b => a | ToUnion[b]
  }

  type IndexOf[E, X] = IndexOf0[E, X, 0]

  type IndexOf0[E, X, I <: Int] <: Int = X match {
    case Unit => -1
    case x *: xs => x match {
      case E => I
      case _ => IndexOf0[E, xs, S[I]]
    }
  }

  inline def summonAsArray[F[_], T]: Array[Any] = inline erasedValue[T] match {
    case _: Unit => Array()
    case _: Tuple1[a] => Array(summon[F[a]])
    case _: (a, b) => Array(summon[F[a]], summon[F[b]])
    case _: (a, b, c) => Array(summon[F[a]], summon[F[b]], summon[F[c]])
    case _: (a, b, c, d) => Array(summon[F[a]], summon[F[b]], summon[F[c]], summon[F[d]])
    case _: (a, b, c, d, e) => Array(summon[F[a]], summon[F[b]], summon[F[c]], summon[F[d]], summon[F[e]])
    // Add fallback for larger sizes
  }

  type LiftP[F[_], T] <: Tuple = T match {
    case Unit => Unit
    case a *: b => F[a] *: LiftP[F, b]
  }

  inline def summonFirst[F[_], T, U]: F[U] = summonFirst0[LiftP[F, T]].asInstanceOf[F[U]]

  inline def summonFirst0[T] <: Any = inline erasedValue[T] match {
    case _: (a *: b) => implicit match {
      case aa: `a` => aa
      case _ => summonFirst0[b]
    }
  }

  given Ops {
    inline def (gen: ProductGeneric[Obj]) toRepr [Obj] (o: Obj): gen.MirroredElemTypes = Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes]
    inline def (gen: ProductGeneric[Obj]) fromRepr [Obj] (r: gen.MirroredElemTypes): Obj = gen.fromProduct(r.asInstanceOf).asInstanceOf[Obj]

    inline def (inst: ProductInstances[F, T]) construct [F[_], T] (f: [t] => F[t] => t): T =
      inst.asInstanceOf[ErasedProductInstances[F[T]]].erasedConstruct(f.asInstanceOf).asInstanceOf
        // Note the necessary cast here
    inline def (inst: ProductInstances[F, T]) map2 [F[_], T] (x: T, y: T)(f: [t] => (F[t], t, t) => t): T =
      inst.asInstanceOf[ErasedProductInstances[F[T]]].erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
        // Note the necessary cast here
  }

  type ProductGenericR[O, R] = Mirror.Product { type MirroredType = O ; type MirroredElemTypes = R }

  inline given mkInstances[F[_], T] as Instances[F, T] given (gen: Generic[T]) =
    inline gen match {
      case p: ProductGeneric[T]   => mkProductInstances[F, T] given p
    }

  inline given mkProductInstances[F[_], T] as ProductInstances[F, T] given (gen: ProductGeneric[T]) =
    new ErasedProductInstances(gen, summonAsArray[F, gen.MirroredElemTypes]).asInstanceOf[ProductInstances[F, T]]

  inline def derive[F[_], T](gen: Generic[T], pg: ProductInstances[F, T] => F[T]): F[T] =
    inline gen match {
      case p: ProductGeneric[T]   => pg(mkProductInstances[F, T] given p)
    }
}

// --------------------------------------------------------------

abstract class ErasedInstances[FT] {
  def erasedMap(x: Any)(f: (Any, Any) => Any): Any
}

final class ErasedProductInstances[FT](val mirror: Mirror.Product, is0: => Array[Any]) extends ErasedInstances[FT] {
  lazy val is = is0

  inline def toProduct(x: Any): Product = x.asInstanceOf[Product]

  class ArrayProduct(val elems: Array[Any]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
  }

  def erasedConstruct(f: Any => Any): Any = {
    val n = is.length
    val arr = new Array[Any](n)
    var i = 0
    while(i < n) {
      arr(i) = f(is(i))
      i = i+1
    }
    mirror.fromProduct(ArrayProduct(arr))
  }

  def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = {
    val n = is.length
    val arr = new Array[Any](n)
    var acc = a
    var i = 0
    while(i < n) {
      val (acc0, e0) = f(acc, is(i))
      e0 match {
        case Some(e) =>
          acc = acc0
          arr(i) = e
        case None =>
          return (acc0, None)
      }
      i = i+1
    }
    (acc, Some(mirror.fromProduct(ArrayProduct(arr))))
  }

  def erasedMap(x0: Any)(f: (Any, Any) => Any): Any = {
    val x = toProduct(x0)
    val n = is.length
    val arr = new Array[Any](n)
    var i = 0
    while(i < n) {
      arr(i) = f(is(i), x.productElement(i))
      i = i+1
    }
    mirror.fromProduct(ArrayProduct(arr))
  }

  def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any = {
    val x = toProduct(x0)
    val y = toProduct(y0)
    val n = is.length
    val arr = new Array[Any](n)
    var i = 0
    while(i < n) {
      arr(i) = f(is(i), x.productElement(i), y.productElement(i))
      i = i+1
    }
    mirror.fromProduct(ArrayProduct(arr))
  }

  def erasedFoldLeft(x0: Any)(i: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = {
    val x = toProduct(x0)
    val n = x.productArity
    @tailrec
    def loop(i: Int, acc: Any): Any =
      if(i >= n) acc
      else
        f(acc, is(i), x.productElement(i)) match {
          case Complete(r) => r
          case Continue(acc) =>
            loop(i+1, acc)
        }

    loop(0, i)
  }

  def erasedFoldLeft2(x0: Any, y0: Any)(i: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = {
    val x = toProduct(x0)
    val y = toProduct(y0)
    val n = x.productArity
    @tailrec
    def loop(i: Int, acc: Any): Any =
      if(i >= n) acc
      else
        f(acc, is(i), x.productElement(i), y.productElement(i)) match {
          case Complete(r) => r
          case Continue(acc) =>
            loop(i+1, acc)
        }

    loop(0, i)
  }
}

// ---------------------------------------------------

type Id[t] = t
type Const[c] = [t] =>> c
case class Wrap[T](t: T)

type ~>[A[_], B[_]] = [t] => A[t] => B[t]

inline def summon[T] = implicit match {
  case t: T => t
}

inline def summonValues[T] <: Tuple = inline erasedValue[T] match {
  case _: Unit => ()
  case _: (a *: b) => constValue[a] *: summonValues[b]
}

inline def summonValuesAsArray[T]: Array[Any] = inline erasedValue[Id[T]] match {
  case _: Unit => Array()
  case _: Tuple1[a] => Array(constValue[a])
  case _: (a, b) => Array(constValue[a], constValue[b])
  case _: (a, b, c) => Array(constValue[a], constValue[b], constValue[c])
  case _: (a, b, c, d) => Array(constValue[a], constValue[b], constValue[c], constValue[d])
  case _: (a, b, c, d, e) => Array(constValue[a], constValue[b], constValue[c], constValue[d], constValue[e])
  // Add fallback for larger sizes
}

case class Labelling[T](label: String, elemLabels: Seq[String])
object Labelling {
  inline given apply[T0] as Labelling[T0] given (mirror: Mirror { type MirroredType = T0 }) =
    Labelling[T0](
      constValue[mirror.MirroredLabel & String],
      WrappedArray.make[String](summonValuesAsArray[mirror.MirroredElemLabels])
    )
}

sealed trait CompleteOr[T]
case class Complete[T](t: T) extends CompleteOr[T]
case class Continue[T](t: T) extends CompleteOr[T]

object Complete {
  inline def apply[T](c: Boolean)(t: T)(f: T): CompleteOr[T] =
    if(c) Complete(t)
    else Continue(f)
}

// ---------------------------------------------------------------------

case class ISB(i: Int) derives Monoid

trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

object Monoid {
  given as Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x+y
  }

  given monoidGen[A] as Monoid[A] given (inst: K0.ProductInstances[Monoid, A]) {
    def empty: A = inst.construct([t] => (ma: Monoid[t]) => ma.empty)
    def combine(x: A, y: A): A = inst.map2(x, y)([t] => (mt: Monoid[t], t0: t, t1: t) => mt.combine(t0, t1))
  }

  inline def derived[A] given (gen: K0.ProductGeneric[A]): Monoid[A] = monoidGen
}


}