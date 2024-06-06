/** 1. Simple type classes with monomorphic implementations and direct extensions.

    trait SemiGroup extends TypeClass {
      def add(that: This): This
    }

    trait Monoid extends SemiGroup
    common {
      def unit: This
    }

    extension IntOps for Int : Monoid {
      def add(that: Int) = this + that
    }
    common {
      def unit = 0
    }

    extension StringOps for String : Monoid {
      def add(that: Int) = this ++ that
    }
    common {
      def unit = ""
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
      xs.foldLeft(Monod.impl[T].unit)(_ `add` _)
*/
object runtime {

  trait TypeClass {
    val commons: TypeClassCommon
    type This = commons.This
  }

  trait TypeClassCommon { self =>
    type This
    type Instance <: TypeClass
    def inject(x: This): Instance { val commons: self.type }
  }

  trait TypeClassCompanion {
    type Impl[T] <: TypeClassCommon { type This = T }
    def impl[T](implicit ev: Impl[T]): Impl[T] = ev
  }

  implicit def inject[From](x: From)
      (implicit ev: TypeClassCommon { type This = From }): ev.Instance { type This = From } =
    ev.inject(x)
}
import runtime.*

object semiGroups {

  trait SemiGroup extends TypeClass {
    val commons: SemiGroupCommon
    import commons.*
    def add(that: This): This
  }
  trait SemiGroupCommon extends TypeClassCommon {
    type Instance <: SemiGroup
  }
  object SemiGroup extends TypeClassCompanion {
    type Impl[T] = SemiGroupCommon { type This = T }
  }

  trait Monoid extends SemiGroup {
    val commons: MonoidCommon
    import commons.*
  }
  trait MonoidCommon extends SemiGroupCommon {
    type Instance <: Monoid
    def unit: This
  }
  object Monoid extends TypeClassCompanion {
    type Impl[T] = MonoidCommon { type This = T }
  }

  implicit object IntOps extends MonoidCommon {
    type This = Int
    type Instance = Monoid
    def unit: Int = 0
    def inject($this: Int) = new Monoid {
      val commons: IntOps.this.type = IntOps.this
      def add(that: this.This): this.This = $this + that
    }
  }

  implicit object StringOps extends MonoidCommon {
    type This = String
    type Instance = Monoid
    def unit = ""
    def inject($this: String) = new Monoid {
      val commons: StringOps.this.type = StringOps.this
      def add(that: this.This): this.This = $this.concat(that)
    }
  }

  enum Nat extends Monoid {
    case Z
    case S(n: Nat)

    def add(that: Nat): Nat = this match {
      case Z => that
      case S(n) => S(n.add(that))
    }

    val commons: Nat.type = Nat
  }
  object Nat extends MonoidCommon {
    type This = Nat
    type Instance = Nat
    def unit = Nat.Z
    def inject($this: Nat) = $this
  }
  import Nat.{Z, S}

  implicit def NatOps: Nat.type = Nat

  def sum[T](xs: List[T])(implicit ev: Monoid.Impl[T]) =
    xs.foldLeft(Monoid.impl[T].unit)((x, y) => x `add` y)

  sum(List(1, 2, 3))
  sum(List("hello ", "world!"))
  sum(List(Z, S(Z), S(S(Z))))
}

/** 2. Generic implementations of simple type classes.

    trait Ord extends TypeClass {
      def compareTo(that: This): Int
      def < (that: This) = compareTo(that) < 0
      def > (that: This) = compareTo(that) > 0
    }
    common {
      val minimum: This
    }

    extension IntOrd for Int : Ord {
      def compareTo(that: Int) =
        if (this < that) -1 else if (this > that) +1 else 0
    }
    common {
      val minimum = Int.MinValue
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
    }
    common {
      val minimum = Nil
    }

    def min[T: Ord](x: T, y: T) = if (x < y) x else y

    def inf[T: Ord](xs: List[T]): T = (Ord.impl[T].minimum /: xs)(min)
*/
object ord {

  trait Ord extends TypeClass {
    val commons: OrdCommon
    import commons.*
    def compareTo(that: This): Int
    def < (that: This) = compareTo(that) < 0
    def > (that: This) = compareTo(that) > 0
  }
  trait OrdCommon extends TypeClassCommon {
    type Instance <: Ord
    def minimum: This
  }
  object Ord extends TypeClassCompanion {
    type Impl[T] = OrdCommon { type This = T }
  }

  implicit object IntOrd extends OrdCommon {
    type This = Int
    type Instance = Ord
    val minimum: Int = Int.MinValue
    def inject($this: Int) = new Ord {
      val commons: IntOrd.this.type = IntOrd.this
      import commons.*
      def compareTo(that: this.This): Int =
        if (this < that) -1 else if (this > that) +1 else 0
    }
  }

  class ListOrd[T](implicit ev: Ord.Impl[T]) extends OrdCommon { self =>
    type This = List[T]
    type Instance = Ord
    def minimum: List[T] = Nil
    def inject($this: List[T]) = new Ord {
      val commons: self.type = self
      import commons.*
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

  implicit def listOrd[T](implicit ev: Ord.Impl[T]): ListOrd[T] =
    new ListOrd[T]

  def min[T](x: T, y: T)(implicit ev: Ord.Impl[T]): T =
    if (x < y) x else y

  def inf[T](xs: List[T])(implicit ev: Ord.Impl[T]): T = {
    val smallest = Ord.impl[T].minimum
    xs.foldLeft(smallest)(min)
  }

  inf(List[Int]())
  inf(List(List(1, 2), List(1, 2, 3)))
  inf(List(List(List(1), List(2)), List(List(1), List(2), List(3))))
}

/** 3. Higher-kinded type classes

    trait Functor[A] extends TypeClass1 {
      def map[B](f: A => B): This[B]
    }
    common {
      def pure[A](x: A): This[A]
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
      static def pure[A] = Nil

      def flatMap[B](f: A => List[B]): List[B] = this match {
        case x :: xs => f(x) ++ xs.flatMap(f)
        case Nil => Nil
      }
    }

    extension MonadFlatten[T[X]: Monad[X]] for T[T[A]] {
      def flatten: T[A] = this.flatMap(identity)
    }
*/
object runtime1 {

  trait TypeClass1 {
    val commons: TypeClassCommon1
    type This = [X] =>> commons.This[X]
  }

  trait TypeClassCommon1 { self =>
    type This[X]
    type Instance[X] <: TypeClass1
    def inject[A](x: This[A]): Instance[A] { val commons: self.type }
  }

  trait TypeClassCompanion1 {
    type Impl[T[_]] <: TypeClassCommon1 { type This = [X] =>> T[X] }
    def impl[T[_]](implicit ev: Impl[T]): Impl[T] = ev
  }

  implicit def inject1[A, From[_]](x: From[A])
      (implicit ev: TypeClassCommon1 {
        type This = [X] =>> From[X]
      }): ev.Instance[A] { type This = [X] =>> From[X] } =
    ev.inject(x)
}
import runtime1.*

object functors {

  trait Functor[A] extends TypeClass1 {
    val commons: FunctorCommon
    import commons.*
    def map[B](f: A => B): This[B]
  }
  trait FunctorCommon extends TypeClassCommon1 {
    type Instance[X] <: Functor[X]
    def pure[A](x: A): This[A]
  }
  object Functor extends TypeClassCompanion1 {
    type Impl[T[_]] = FunctorCommon { type This = [X] =>> T[X] }
  }

  trait Monad[A] extends Functor[A] {
    val commons: MonadCommon
    import commons.*
    def flatMap[B](f: A => This[B]): This[B]
    def map[B](f: A => B) = this.flatMap(f.andThen(commons.pure))
  }
  trait MonadCommon extends FunctorCommon {
    type Instance[X] <: Monad[X]
  }
  object Monad extends TypeClassCompanion1 {
    type Impl[T[_]] = MonadCommon { type This = [X] =>> T[X] }
  }

  def develop[A, F[X]](n: Int, x: A, f: A => A)(implicit ev: Functor.Impl[F]): F[A] =
    if (n == 0) Functor.impl[F].pure(x)
    else develop(n - 1, x, f).map(f).asInstanceOf

  implicit object ListMonad extends MonadCommon {
    type This[+X] = List[X]
    type Instance[X] = Monad[X]
    def pure[A](x: A) = x :: Nil
    def inject[A]($this: List[A]) = new Monad[A] {
      val commons: ListMonad.this.type = ListMonad
      import commons.*
      def flatMap[B](f: A => List[B]): List[B] = $this.flatMap(f)
    }
  }

  object MonadFlatten {
    def flattened[T[_], A]($this: T[T[A]])(implicit ev: Monad.Impl[T]): T[A] =
      ??? // $this.flatMap[A](identity)   disabled since it does not typecheck
  }

  MonadFlatten.flattened(List(List(1, 2, 3), List(4, 5))) // ok, synthesizes (using ListMonad)
  MonadFlatten.flattened(List(List(1, 2, 3), List(4, 5)))(using ListMonad) // was an error
  /*
  When checking `ListMonad <:< functors.Monad.Impl[T]`
  we eventually get to the comparison `[X] =>> T[X] <:< [+X] =>> List[X]`
  because the `This` type member of `ListMonad` has a covariance annotation.
  This fails the variance conformance checks despite the fact that T has been instantiated to List,
  since it has been substituted into the refinement (and cached) before its instantiation.
  */
}