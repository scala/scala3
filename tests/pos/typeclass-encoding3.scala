/*
  [T : M]  =  [T] ... (implicit ev: Injectable[T, M])       if M is a normal trait
           =  [T] ... (implicit ev: M.Impl[T])              if M is an extension trait

      M.Impl[T] = Common { type This = T }
      M.Impl[T[_]] = Common { type This = T }
      ...

      Extension traits of different kinds cannot be mixed.
      Extension traits can extend normal traits.
      Normal traits cannot extend extension traits.
*/
object runtime {

  trait TypeClass {
    /** The companion object of the implementing type */
    val `common`: TypeClass.Common
  }

  trait Injector {
    type This
    type Instance
    implicit def inject(x: This): Instance
  }

  type Injectable[T, +U] = Injector { type This = T; type Instance <: U }

  implicit def selfInject[U, T <: U]: Injectable[T, U] =
    new Injector{
      type This = T
      type Instance = U
      implicit def inject(x: This): Instance = x
    }

  object TypeClass {

    /** Base trait for companion objects of all implementations of this typeclass */
    trait Common extends Injector { self =>

      /** The implementating type */
      type This

      /** The implemented typeclass */
      type Instance <: TypeClass { val `common`: self.type }

      /** Create instances of typeclass from instances of the implementing type */
      implicit def inject(x: This): Instance
    }

    /** Base trait for the companion objects of type classes themselves  */
    trait Companion {

      /** The `Common` base trait defining common (static) operations of this typeclass */
      type Common <: TypeClass.Common

      /** Helper type to characterize implementations via type `T` for this typeclass */
      type Impl[T] = Common { type This = T }

      /** The implementation via type `T` for this typeclass, as found by implicit search */
      def impl[T](implicit ev: Impl[T]): Impl[T] = ev
    }
  }

  implicit def injectTypeClass[From, U](x: From)
    (implicit ev: Injector { type This = From }): ev.Instance =
      ev.inject(x)
}

/** 0. All combinations of

    - trait / trait with common / extension trait
    - extending class / extension
    - monomorphic / generic implementation

    trait HasLength {
      def length: Int
    }

    trait HasBoundedLength extends HasLength {
      common def limit: Int
    }

    extension trait HasBoundedLengthX extends HasBoundedLength {
      common def longest: This
    }

    class C1(xs: Array[Int]) extends HasLength {
      def length = xs.length
    }

    class CG1[T](xs: Array[T]) extends HasLength {
      def length = xs.length
    }

    class C2(xs: Array[Int]) extends C1(xs) with HasBoundedLength {
      common def limit = 100
    }

    class CG2[T](xs: Array[Int]) extends CG1[T](xs) with HasBoundedLength {
      common def limit = 100
    }

    final class C3(xs: Array[Int]) extends C2(xs) with HasBoundedLengthX {
      common def longest = new C3(new Array[Int](limit))
    }

    final class CG3[T](xs: Array[T])(implicit tag: ClassTag[T]) extends CG2[T](xs) with HasBoundedLengthX {
      common def longest = new CG3(new Array[T](limit))
    }

    class D1(val xs: Array[Int])
    class DG1[T](val xs: Array[T])

    class D2(val xs: Array[Int])
    class DG2[T](val xs: Array[T])

    class D3(val xs: Array[Int])
    class DG3[T](val xs: Array[T])

    extension DHasLength for D1 : HasLength {
      def length = xs.length
    }

    extension DGHasLength[T] for DG1[T] : HasLength {
      def length = xs.length
    }

    extension DHasBoundedLength for D2 : HasBoundedLength {
      def length = xs.length
      common def limit = 100
    }

    extension DGHasBoundedLength[T] for DG2[T] : HasBoundedLength {
      def length = xs.length
      common def limit = 100
    }

    extension DHasBoundedLengthX for D3 : HasBoundedLengthX {
      def length = xs.length
      common def limit = 100
      common def longest = new D3(new Array[Int](limit))
    }

    extension DGHasBoundedLengthX[T](implicit tag: ClassTag[T]) for DG3[T] : HasBoundedLengthX {
      def length = xs.length
      common def limit = 100
      common def longest = new DG3(new Array[T](limit))
    }

    def length[T : HasLength](x: T) = x.length

    def lengthOK[T : HasBoundedLength](x: T) =
      x.length < x.common.limit

    def lengthOKX[T : HasBoundedLengthX](x: T) =
      x.length < HasBoundedLengthX.impl[T].limit

    def longestLengthOK[T : HasBoundedLengthX](implicit tag: ClassTag[T]) = {
      val impl = HasBoundedLengthX.impl[T]
      impl.longest.length < impl.limit
    }

    val xs = Array(1, 2, 3)
    val c1 = new C1(xs)
    val cg1 = new CG1(xs)
    val c2 = new C2(xs)
    val cg2 = new CG2(xs)
    val c3 = new C3(xs)
    val cg3 = new CG3(xs)

    val d1 = new D1(xs)
    val dg1 = new DG1(xs)
    val d2 = new D2(xs)
    val dg2 = new DG2(xs)
    val d3 = new D3(xs)
    val dg3 = new DG3(xs)

    length(c1)
    length(cg1)
    length(c2)
    length(cg2)
    length(c3)
    length(cg3)

    length(d1)
    length(dg1)
    length(d2)
    length(dg2)
    length(d3)
    length(dg3)

    lengthOK(c2)
    lengthOK(cg2)
    lengthOK(c3)
    lengthOK(cg3)

    lengthOK(d2)
    lengthOK(dg2)
    lengthOK(d3)
    lengthOK(dg3)

    lengthOKX(c3)
    lengthOKX(cg3)

    lengthOKX(d3)
    lengthOKX(dg3)

    longestLengthOK(c3)
    longestLengthOK(cg3)
    longestLengthOK(d3)
    longestLengthOK(cg3)
*/
object hasLength {
  import runtime._
  import reflect.ClassTag

  trait HasLength {
    def length: Int
  }

  trait HasBoundedLength extends HasLength {
    val `common`: HasBoundedLength.Common
    import `common`._
  }

  object HasBoundedLength {
    trait Common {
      def limit: Int
    }
  }

  trait HasBoundedLengthX extends HasBoundedLength with TypeClass {
    val `common`: HasBoundedLengthX.Common
    import `common`._
  }

  object HasBoundedLengthX extends TypeClass.Companion {
    trait  Common extends HasBoundedLength.Common with TypeClass.Common { self =>
      type Instance <: HasBoundedLengthX { val `common`: self.type }
      def limit: Int
      def longest: This
    }
  }

  class C1(xs: Array[Int]) extends HasLength {
    def length = xs.length
  }

  class CG1[T](xs: Array[T]) extends HasLength {
    def length = xs.length
  }

  class C2(xs: Array[Int]) extends C1(xs) with HasBoundedLength {
    val `common`: C2Common = C2
    import `common`._
  }
  class C2Common extends HasBoundedLength.Common {
    def limit = 100
  }
  object C2 extends C2Common

  class CG2[T](xs: Array[T]) extends CG1(xs) with HasBoundedLength {
    val `common`: CG2Common[T] = CG2[T]
    import `common`._
  }
  class CG2Common[T] extends HasBoundedLength.Common {
    def limit = 100
  }
  object CG2 {
    def apply[T] = new CG2Common[T]
  }

  class C3(xs: Array[Int]) extends C2(xs) with HasBoundedLengthX {
    override val `common`: C3Common = C3
    import `common`._
  }
  class C3Common extends C2Common with HasBoundedLengthX.Common { self =>
    type This = C3
    type Instance = C3 { val `common`: self.type }
    def inject(x: This): Instance = x.asInstanceOf

    def longest = new C3(new Array[Int](limit))
  }
  implicit object C3 extends C3Common

  class CG3[T](xs: Array[T])(implicit tag: ClassTag[T]) extends CG2(xs) with HasBoundedLengthX {
    override val `common`: CG3Common[T] = CG3[T]
    import `common`._
  }
  class CG3Common[T](implicit tag: ClassTag[T]) extends CG2Common[T] with HasBoundedLengthX.Common { self =>
    type This = CG3[T]
    type Instance = CG3[T] { val `common`: self.type }
    def inject(x: This): Instance = x.asInstanceOf

    def longest = new CG3(new Array[T](limit))
  }
  object CG3 {
    def apply[T](implicit tag: ClassTag[T]) = new CG3Common[T]
  }
  implicit def CG3Common[T](implicit tag: ClassTag[T]): CG3Common[T] = CG3[T]

  class D1(val xs: Array[Int])
  class DG1[T](val xs: Array[T])

  class D2(val xs: Array[Int])
  class DG2[T](val xs: Array[T])

  class D3(val xs: Array[Int])
  class DG3[T](val xs: Array[T])

  implicit object DHasLength extends Injector {
    type This = D1
    type Instance = HasLength
    def inject(x: D1) = new HasLength {
      def length = xs.length
    }
  }

  class DGHasLength[T] extends Injector {
    type This = DG1[T]
    type Instance = HasLength
    def inject(x: DG1[T]) = new HasLength {
      def length = xs.length
    }
  }
  implicit def DGHasLength[T]: DGHasLength[T] = new DGHasLength

  implicit object DHasBoundedLength extends Injector with HasBoundedLength.Common { self =>
    type This = D2
    type Instance = HasBoundedLength
    def inject(x: D2) = new HasBoundedLength {
      val `common`: self.type = self
      import `common`._
      def length = xs.length
    }
    def limit = 100
  }

  class DGHasBoundedLength[T] extends Injector with HasBoundedLength.Common { self =>
    type This = DG2[T]
    type Instance = HasBoundedLength
    def inject(x: DG2[T]) = new HasBoundedLength {
      val `common`: self.type = self
      import `common`._
      def length = xs.length
    }
    def limit = 100
  }
  implicit def DGHasBoundedLength[T]: DGHasBoundedLength[T] = new DGHasBoundedLength

  implicit object DHasBoundedLengthX extends HasBoundedLengthX.Common { self =>
    type This = D3
    type Instance = HasBoundedLengthX { val `common`: self.type }
    def inject(x: D3) = new HasBoundedLengthX {
      val `common`: self.type = self
      import `common`._
      def length = xs.length
    }
    def limit = 100
    def longest = new D3(new Array[Int](limit))
  }

  class DGHasBoundedLengthX[T](implicit tag: ClassTag[T]) extends HasBoundedLengthX.Common { self =>
    type This = DG3[T]
    type Instance = HasBoundedLengthX { val `common`: self.type }
    def inject(x: DG3[T]) = new HasBoundedLengthX {
      val `common`: self.type = self
      import `common`._
      def length = xs.length
    }
    def limit = 100
    def longest = new DG3(new Array[T](limit))
  }
  implicit def DGHasBoundedLengthX[T](implicit tag: ClassTag[T]): DGHasBoundedLengthX[T] = new DGHasBoundedLengthX

  def length[T](x: T)(implicit ev: Injectable[T, HasLength]) = x.length

  def lengthOK[T](x: T)(implicit ev: Injectable[T, HasBoundedLength]) =
    x.length < x.common.limit

  def lengthOKX[T](x: T)(implicit ev: HasBoundedLengthX.Impl[T]) =
    x.length < HasBoundedLengthX.impl[T].limit

  def longestLengthOK[T](implicit ev: HasBoundedLengthX.Impl[T], tag: ClassTag[T]) = {
    val impl = HasBoundedLengthX.impl[T]
    impl.longest.length < impl.limit
  }

  val xs = Array(1, 2, 3)
  val c1 = new C1(xs)
  val cg1 = new CG1(xs)
  val c2 = new C2(xs)
  val cg2 = new CG2(xs)
  val c3 = new C3(xs)
  val cg3 = new CG3(xs)

  val d1 = new D1(xs)
  val dg1 = new DG1(xs)
  val d2 = new D2(xs)
  val dg2 = new DG2(xs)
  val d3 = new D3(xs)
  val dg3 = new DG3(xs)

  length(c1)
  length(cg1)
  length(c2)
  length(cg2)
  length(c3)(selfInject)
  length(cg3)(selfInject)

  length(d1)
  length(dg1)
  length(d2)
  length(dg2)
  length(d3)
  length(dg3)

  lengthOK(c2)
  lengthOK(cg2)
  lengthOK(c3)(selfInject)
  lengthOK(cg3)(selfInject)

  lengthOK(d2)
  lengthOK(dg2)
  lengthOK(d3)
  lengthOK(dg3)

  lengthOKX(c3)
  lengthOKX(cg3)

  lengthOKX(d3)
  lengthOKX(dg3)

  longestLengthOK[C3]
  longestLengthOK[CG3[Int]]
  longestLengthOK[D3]
  longestLengthOK[DG3[Int]]
}
/** 1. Simple type classes with monomorphic implementations and direct extensions.

    extension trait SemiGroup {
      def add(that: This): This
      def add2(that: This): This = add(that).add(that)
    }

    extension trait Monoid extends SemiGroup {
      common def unit: This
    }

    extension IntOps for Int : Monoid {
      def add(that: Int) = this + that
      common def unit = 0
    }

    extension StringOps for String : Monoid {
      def add(that: Int) = this ++ that
      common def unit = ""
    }

    enum Nat extends Monoid {
      case Z
      case S(n: Nat)

      def add(that: Nat): Nat = this match {
        case S => that
        case S(n) => S(n.add(that))
      }
    }
    common {
      def unit = Z
    }

    def sum[T: Monoid](xs: List[T]): T =
      (Monod.impl[T].unit /: xs)(_ `add` _)
*/

import runtime._

object semiGroups {

  trait SemiGroup extends TypeClass {
    val `common`: SemiGroup.Common
    import `common`._
    def add(that: This): This
    def add2(that: This): This = add(that).add(that)
  }

  object SemiGroup extends TypeClass.Companion {
    trait Common extends TypeClass.Common { self =>
      type Instance <: SemiGroup { val `common`: self.type }
    }
  }

  trait Monoid extends SemiGroup {
    val `common`: Monoid.Common
    import `common`._
  }
  object Monoid extends TypeClass.Companion {
    trait Common extends SemiGroup.Common { self =>
      type Instance <: Monoid { val `common`: self.type }
      def unit: This
    }
  }

  implicit object IntOps extends Monoid.Common {
    type This = Int
    type Instance = Monoid { val `common`: IntOps.type }
    def unit: Int = 0
    def inject($this: Int) = new Monoid {
      val `common`: IntOps.this.type = IntOps.this
      def add(that: This): This = $this + that
    }
  }

  implicit object StringOps extends Monoid.Common {
    type This = String
    type Instance = Monoid { val `common`: StringOps.type }
    def unit = ""
    def inject($this: String) = new Monoid {
      val `common`: StringOps.this.type = StringOps.this
      def add(that: This): This = $this.concat(that)
    }
  }

  enum Nat extends Monoid {
    case Z
    case S(n: Nat)

    def add(that: Nat): Nat = this match {
      case Z => that
      case S(n) => S(n.add(that))
    }

    val `common`: Nat.type = Nat
  }
  object Nat extends Monoid.Common {
    type This = Nat
    type Instance = Nat
    def unit = Nat.Z
    def inject($this: Nat) = $this
  }
  import Nat.{Z, S}

  implicit def NatOps: Nat.type = Nat

  def sum[T](xs: List[T])(implicit $ev: Monoid.Impl[T]) =
    (Monoid.impl[T].unit /: xs)((x, y) => x `add` y)

  sum(List(1, 2, 3))
  sum(List("hello ", "world!"))
  sum(List(Z, S(Z), S(S(Z))))
}

/** 2. Generic implementations of simple type classes.

    extension trait Ord {
      def compareTo(that: This): Int
      def < (that: This) = compareTo(that) < 0
      def > (that: This) = compareTo(that) > 0

      common def minimum: This
    }

    extension IntOrd for Int : Ord {
      def compareTo(that: Int) =
        if (this < that) -1 else if (this > that) +1 else 0

      common def minimum = Int.MinValue
    }

    extension ListOrd[T : Ord] for List[T] : Ord {
      def compareTo(that: List[T]): Int = (this, that) match {
        case (Nil, Nil) => 0
        case (Nil, _) => -1
        case (_, Nil) => +1
        case (x :: xs, y :: ys) =>
          val fst = x.compareTo(y)
          if (fst != 0) fst else xs.compareTo(ys)
      }

      common def minimum: List[T] = Nil
    }

    def min[T: Ord](x: T, y: T) = if (x < y) x else y

    def inf[T: Ord](xs: List[T]): T = (Ord.impl[T].minimum /: xs)(min)
*/
object ord {

  trait Ord extends TypeClass {
    val `common`: Ord.Common
    import `common`._
    def compareTo(that: This): Int
    def < (that: This) = compareTo(that) < 0
    def > (that: This) = compareTo(that) > 0
  }
  object Ord extends TypeClass.Companion {
    trait Common extends TypeClass.Common { self =>
      type Instance <: Ord { val `common`: self.type }
      def minimum: This
    }
  }

  implicit object IntOrd extends Ord.Common {
    type This = Int
    type Instance = Ord { val `common`: IntOrd.type }
    val minimum: Int = Int.MinValue
    def inject($this: Int) = new Ord {
      val `common`: IntOrd.this.type = IntOrd.this
      import `common`._
      def compareTo(that: This): Int =
        if (this < that) -1 else if (this > that) +1 else 0
    }
  }

  class ListOrd[T](implicit $ev: Ord.Impl[T]) extends Ord.Common { self =>
    type This = List[T]
    type Instance = Ord { val `common`: self.type }
    def minimum: List[T] = Nil
    def inject($this: List[T]) = new Ord {
      val `common`: self.type = self
      import `common`._
      def compareTo(that: List[T]): Int = ($this, that) match {
        case (Nil, Nil) => 0
        case (Nil, _) => -1
        case (_, Nil) => +1
        case (x :: xs, y :: ys) =>
          val fst = x.compareTo(y)
          if (fst != 0) fst else xs.compareTo(ys)
      }
    }
  }

  implicit def listOrd[T](implicit $ev: Ord.Impl[T]): ListOrd[T] =
    new ListOrd[T]

  def min[T](x: T, y: T)(implicit $ev: Ord.Impl[T]): T =
    if (x < y) x else y

  def inf[T](xs: List[T])(implicit $ev: Ord.Impl[T]): T = {
    val smallest = Ord.impl[T].minimum
    (smallest /: xs)(min)
  }

  inf(List[Int]())
  inf(List(List(1, 2), List(1, 2, 3)))
  inf(List(List(List(1), List(2)), List(List(1), List(2), List(3))))
}

/** 3. Higher-kinded type classes

    extension trait Functor[A] extends TypeClass1 {
      def map[B](f: A => B): This[B]

      common def pure[A](x: A): This[A]
    }

    // Generically, `pure[A]{.map(f)}^n`
    def develop[A, F[_] : Functor](n: Int, f: A => A): F[A] =
      if (n == 0) Functor.impl[F].pure[A]
      else develop[A, F](n - 1, f).map(f)

    extension trait Monad[A] extends Functor[A] {
      def flatMap[B](f: A => This[B]): This[B]
      def map[B](f: A => B) = this.flatMap(f.andThen(pure))
    }

    extension ListMonad[T] for List[T] : Monad[T] {
      def flatMap[B](f: A => List[B]): List[B] = this match {
        case x :: xs => f(x) ++ xs.flatMap(f)
        case Nil => Nil
      }
      common def pure[A] = Nil
    }

    extension MonadFlatten[T[_]: Monad] for T[T[A]] {
      def flatten: T[A] = this.flatMap(identity)
    }
*/
object runtime1 {

  trait TypeClass1[X] {
    val `common`: TypeClass1.Common
  }
  object TypeClass1 {
    trait Common { self =>
      type This[X]
      type Instance[X] <: TypeClass1[X] { val `common`: self.type }
      def inject[A](x: This[A]): Instance[A]
    }

    trait Companion {
      type Common <: TypeClass1.Common
      type Impl[T[_]] = Common { type This = T }
      def impl[T[_]](implicit ev: Impl[T]): Impl[T] = ev
    }
  }

  implicit def decorateTypeClass1[A, From[_]](x: From[A])
      (implicit ev: TypeClass1.Common { type This = From }): ev.Instance[A] =
    ev.inject(x)
}
import runtime1._

object functors {

  trait Functor[A] extends TypeClass1[A] {
    val `common`: Functor.Common
    import `common`._
    def map[B](f: A => B): This[B]
  }
  object Functor extends TypeClass1.Companion {
    trait Common extends TypeClass1.Common { self =>
      type Instance[X] <: Functor[X] { val `common`: self.type }
      def pure[A](x: A): This[A]
    }
  }

  trait Monad[A] extends Functor[A] {
    val `common`: Monad.Common
    import `common`._
    def flatMap[B](f: A => This[B]): This[B]
    def map[B](f: A => B) = this.flatMap(f.andThen(`common`.pure))
  }
  object Monad extends TypeClass1.Companion {
    trait Common extends Functor.Common { self =>
      type Instance[X] <: Monad[X] { val `common`: self.type }
    }
  }

  def develop[A, F[X]](n: Int, x: A, f: A => A)(implicit $ev: Functor.Impl[F]): F[A] =
    if (n == 0) Functor.impl[F].pure(x)
    else develop(n - 1, x, f).map(f)

  implicit object ListMonad extends Monad.Common {
    type This = List
    type Instance[X] = Monad[X] { val `common`: ListMonad.type }
    def pure[A](x: A) = x :: Nil
    def inject[A]($this: List[A]) = new Monad[A] {
      val `common`: ListMonad.this.type = ListMonad
      import `common`._
      def flatMap[B](f: A => List[B]): List[B] = $this.flatMap(f)
    }
  }

  object MonadFlatten {
    def flattened[T[_], A]($this: T[T[A]])(implicit $ev: Monad.Impl[T]): T[A] =
      $this.flatMap(identity  )
  }

  MonadFlatten.flattened(List(List(1, 2, 3), List(4, 5)))
}
