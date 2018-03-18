/** A possible type class encoding for

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

    def sum[T: Monoid](xs: List[T]): T =
      (Monod.impl[T].unit /: xs)(_ `add` _)
*/
object runtime {

  trait TypeClass {
    val common: TypeClassCommon
    type This = common.This
  }

  trait TypeClassCommon { self =>
    type This
    type Instance <: TypeClass
    def inject(x: This): Instance { val common: self.type }
  }

  trait TypeClassCompanion {
    type Impl[T] <: Extension[T, _]
    def impl[T](implicit ev: Impl[T]): Impl[T] = ev
  }

  trait Extension[From, To <: TypeClass] extends TypeClassCommon {
    type This = From
    type Instance = To
  }

  implicit def inject[From](x: From)
      (implicit ev: Extension[From, _]): ev.Instance { type This = From } =
    ev.inject(x)
}
import runtime._

object semiGroups {

  trait SemiGroup extends TypeClass {
    val common: SemiGroupCommon
    import common._
    def add(that: This): This
  }
  trait SemiGroupCommon extends TypeClassCommon {
    type Instance <: SemiGroup
  }
  object SemiGroup extends TypeClassCompanion {
    type Impl[T] = Extension[T, SemiGroup] with SemiGroupCommon
  }

  trait Monoid extends SemiGroup {
    val common: MonoidCommon
    import common._
  }
  trait MonoidCommon extends SemiGroupCommon {
    type Instance <: Monoid
    def unit: This
  }
  object Monoid extends TypeClassCompanion {
    type Impl[T] = Extension[T, Monoid] with MonoidCommon
  }

  implicit object IntOps extends Extension[Int, Monoid] with MonoidCommon {
    type This = Int
    type Instance = Monoid
    def unit: Int = 0
    def inject($this: Int) = new Monoid {
      val common: IntOps.this.type = IntOps.this
      def add(that: This): This = $this + that
    }
  }

  implicit object StringOps extends Extension[String, Monoid] with MonoidCommon {
    type This = String
    type Instance = Monoid
    def unit = ""
    def inject($this: String) = new Monoid {
      val common: StringOps.this.type = StringOps.this
      def add(that: This): This = $this.concat(that)
    }
  }

  def sum[T](xs: List[T])(implicit $ev: Monoid.Impl[T]) =
    (Monoid.impl[T].unit /: xs)((x, y) => x `add` y)
}

/** Encoding for

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
    val common: OrdCommon
    import common._
    def compareTo(that: This): Int
    def < (that: This) = compareTo(that) < 0
    def > (that: This) = compareTo(that) > 0
  }
  trait OrdCommon extends TypeClassCommon {
    type Instance <: Ord
    def minimum: This
  }
  object Ord extends TypeClassCompanion {
    type Impl[T] = Extension[T, Ord] with OrdCommon
  }

  implicit object IntOrd extends Extension[Int, Ord] with OrdCommon {
    type This = Int
    type Instance = Ord
    val minimum: Int = Int.MinValue
    def inject($this: Int) = new Ord {
      val common: IntOrd.this.type = IntOrd.this
      def compareTo(that: This): Int =
        if (this < that) -1 else if (this > that) +1 else 0
    }
  }

  class ListOrd[T](implicit ev: Ord.Impl[T])
  extends Extension[List[T], Ord] with OrdCommon { self =>
    type This = List[T]
    type Instance = Ord
    def minimum: List[T] = Nil
    def inject($this: List[T]) = new Ord {
      val common: self.type = self
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
    (smallest /: xs)(min)
  }
}

/** Encoding for

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
    val common: TypeClassCommon1
    type This = common.This
  }

  trait TypeClassCommon1 { self =>
    type This[X]
    type Instance[X] <: TypeClass1
    def inject[A](x: This[A]): Instance[A] { val common: self.type }
  }

  trait TypeClassCompanion1 {
    type Impl[T[_]] <: Extension1[T, _]
    def impl[T[_]](implicit ev: Impl[T]): Impl[T] = ev
  }

  trait Extension1[From[_], To[X] <: TypeClass1] extends TypeClassCommon1 {
    type This[X] = From[X]
    type Instance[X] = To[X]
  }

  implicit def inject1[A, From[_]](x: From[A])
      (implicit ev: Extension1[From, _]): ev.Instance[A] { type This = From } =
    ev.inject(x)
}
import runtime1._

object functors {

  trait Functor[A] extends TypeClass1 {
    val common: FunctorCommon
    import common._
    def map[B](f: A => B): This[B]
  }
  trait FunctorCommon extends TypeClassCommon1 {
    type Instance[X] <: Functor[X]
    def pure[A](x: A): This[A]
  }
  object Functor extends TypeClassCompanion1 {
    type Impl[T[_]] = Extension1[T, Functor] with FunctorCommon
  }

  trait Monad[A] extends Functor[A] {
    val common: MonadCommon
    import common._
    def flatMap[B](f: A => This[B]): This[B]
    def map[B](f: A => B) = this.flatMap(f.andThen(common.pure))
  }
  trait MonadCommon extends FunctorCommon {
    type Instance[X] <: Monad[X]
  }
  object Monad extends TypeClassCompanion1 {
    type Impl[T[_]] = Extension1[T, Monad] with MonadCommon
  }

  def develop[A, F[X]](n: Int, x: A, f: A => A)(implicit ev: Functor.Impl[F]): F[A] =
    if (n == 0) Functor.impl[F].pure(x)
    else develop(n - 1, x, f).map(f)

  implicit object ListMonad extends Extension1[List, Monad] with MonadCommon {
    type This[A] = List[A]
    type Instance = Monad
    def pure[A](x: A) = x :: Nil
    def inject[A]($this: List[A]) = new Monad[A] {
      val common: ListMonad.this.type = ListMonad
      def flatMap[B](f: A => List[B]): List[B] = $this.flatMap(f)
    }
  }

  object MonadFlatten {
    def flatten[T[_], A]($this: T[T[A]])(implicit ev: Monad.Impl[T]): T[A] =
      $this.flatMap(identity  )
  }
}