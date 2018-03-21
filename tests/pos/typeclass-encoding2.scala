/** 1. Simple type classes with monomorphic implementations and direct extensions.

    trait SemiGroup extends TypeClass {
      def add(that: This): This
      def add2(that: This): This = add(that).add(that)
    }

    trait Monoid extends SemiGroup {
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
object runtime {

  trait TypeClass {
    /** The companion object of the implementing type */
    val `common`: TypeClass.Common
  }

  object TypeClass {

    /** Base trait for companion objects of all implementations of this typeclass */
    trait Common { self =>

      /** The implementating type */
      type This

      /** The implemented typeclass */
      type Instance <: TypeClass { val `common`: self.type }

      /** Create instances of typeclass from instances of the implementing type */
      implicit def decorate(x: This): Instance
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

  implicit def decorateTypeClass[From](x: From)
      (implicit ev: TypeClass.Common { type This = From }): ev.Instance =
    ev.decorate(x)
}
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
    def decorate($this: Int) = new Monoid {
      val `common`: IntOps.this.type = IntOps.this
      def add(that: This): This = $this + that
    }
  }

  implicit object StringOps extends Monoid.Common {
    type This = String
    type Instance = Monoid { val `common`: StringOps.type }
    def unit = ""
    def decorate($this: String) = new Monoid {
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
    def decorate($this: Nat) = $this
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

    trait Ord extends TypeClass {
      def compareTo(that: This): Int
      def < (that: This) = compareTo(that) < 0
      def > (that: This) = compareTo(that) > 0

      common val minimum: This
    }

    extension IntOrd for Int : Ord {
      def compareTo(that: Int) =
        if (this < that) -1 else if (this > that) +1 else 0

      common val minimum = Int.MinValue
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

      common val minimum = Nil
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
    def decorate($this: Int) = new Ord {
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
    def decorate($this: List[T]) = new Ord {
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

    trait Functor[A] extends TypeClass1 {
      def map[B](f: A => B): This[B]

      common def pure[A](x: A): This[A]
    }

    // Generically, `pure[A]{.map(f)}^n`
    def develop[A, F[X] : Functor[X]](n: Int, f: A => A): F[A] =
      if (n == 0) Functor.impl[F].pure[A]
      else develop[A, F](n - 1, f).map(f)

    trait Monad[A] extends Functor[A] {
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

    extension MonadFlatten[T[X]: Monad[X]] for T[T[A]] {
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
      def decorate[A](x: This[A]): Instance[A]
    }

    trait Companion {
      type Common <: TypeClass1.Common
      type Impl[T[_]] = Common { type This = T }
      def impl[T[_]](implicit ev: Impl[T]): Impl[T] = ev
    }
  }

  implicit def decorateTypeClass1[A, From[_]](x: From[A])
      (implicit ev: TypeClass1.Common { type This = From }): ev.Instance[A] =
    ev.decorate(x)
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
    def decorate[A]($this: List[A]) = new Monad[A] {
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
