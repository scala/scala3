object Test {

/* --------------------------------------------------------------------------

  Extension Methods & Simple Interfaces

  case class Reactangle(width: Double, height: Double)
  case class Circle(radius: Double)

  extension for Rectangle
    def area: Double = width * height

  trait HasArea
    def area: Double

  extension for Circle : HasArea
    def area = radius * 2 * math.Pi

* ------------------------------------------------------------------------ */

  case class Rectangle(width: Double, height: Double)
  case class Circle(radius: Double)

  implicit class Rectangle_area(`this`: Rectangle) {
    def area: Double = `this`.width * `this`.height
  }

  implicit class Circle_HasArea(`this`: Circle) {
    def area = `this`.radius * 2 * math.Pi
  }

/* --------------------------------------------------------------------------

  Monomorphic:

  trait SemiGroup
    def combine(that: This): This
  common
    type This

  trait Monoid extends SemiGroup
  common
    def unit: This

  class Str(val s: String) extends Monoid
    def combine(that: Str): Str = new Str(this.s + that.s)
  object Str:
    def unit = ""

  extension for String : Monoid
    def combine(that: String): String = this + that
  common
    def unit = ""

  def f[X: Monoid](x: X) = Monoid.by[X].unit.combine(x)

  def sum[T: Monoid](xs: List[T]): T =
    xs.foldLeft(Monoid.by[T].unit)(_ `combine` _)

* ---------------------------------------------------------------------------- */

  trait SemiGroup {
    val common: SemiGroup.Common
    import common._

    def combine(that: This): This
  }
  object SemiGroup {
    trait Common { self =>
      type This
      def inject(x: This): SemiGroup  { val common: self.type }
    }
    def by[A](implicit ev: SemiGroup.Common { type This = A }): ev.type = ev
  }

  trait Monoid extends SemiGroup {
    val common: Monoid.Common
    import common._
  }
  object Monoid {
    trait Common extends SemiGroup.Common { self =>
      def inject(x: This): Monoid { val common: self.type }
      def unit: This
    }
    def by[A](implicit ev: Monoid.Common { type This = A }): ev.type = ev
  }

  class Str(val s: String) extends Monoid {
    val common: Str.type = Str
    def combine(that: Str): Str = new Str(this.s + that.s)
  }
  object Str extends Monoid.Common {
    type This = Str
    def inject(x: This): Str = x
    def unit = new Str("")
  }

  class SubStr(s: String) extends Str(s)

  implicit object String_Monoid extends Monoid.Common { self =>
    type This = String
    class Impl(`this`: This) extends Monoid {
      val common: self.type = self
      def combine(that: String): String = `this` + that
    }
    def inject(x: This): Impl = new Impl(x)
    def unit = ""
  }

  def f[X](x: X)(implicit ev: Monoid.Common { type This = X }) = {
    ev.inject(Monoid.by[X].unit).combine(x)
  }

  def sum[A](xs: List[A])(implicit ev: Monoid.Common { type This = A }): A =
    xs.foldLeft(Monoid.by[A].unit)(ev.inject(_).combine(_))

  f(new Str("abc"))(Str)
  f("abc")
  f(new SubStr("abc"))(Str)

  sum(List("A", "B", "C"))

/* --------------------------------------------------------------------------

  Generic:

  trait Ord
    def compareTo(that: This): Int
    def < (that: This) = compareTo(that) < 0
    def > (that: This) = compareTo(that) > 0
  common
    val minimum: This

  extension for Int : Ord
    def compareTo(that: Int) =
      if (this < that) -1 else if (this > that) +1 else 0
  common
    val minimum = Int.MinValue

  extension [T : Ord] for List[T] : Ord:
    def compareTo(that: List[T]): Int = (this, that) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs, y :: ys) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs.compareTo(ys)
  common
    val minimum = Nil

  def max[T: Ord](x: T, y: T) = if (x < y) y else x

  def max[T: Ord](xs: List[T]): T = (Ord.by[T].minimum /: xs)(max(_, _))

* ---------------------------------------------------------------------------- */

  trait Ord {
    val common: Ord.Common
    import common._

    def compareTo(that: This): Int
    def < (that: This) = compareTo(that) < 0
    def > (that: This) = compareTo(that) > 0
  }
  object Ord {
    trait Common { self =>
      type This
      def inject(x: This): Ord  { val common: self.type }
      val minimum: This
    }
    def by[A](implicit ev: Ord.Common { type This = A }): ev.type = ev
  }

  implicit object Int_Ord extends Ord.Common { self =>
    type This = Int
    class Impl(`this`: This) extends Ord {
      val common: self.type = self
      def compareTo(that: Int) =
        if (`this` < that) -1 else if (`this` > that) +1 else 0
    }
    def inject(x: This): Impl = new Impl(x)
    val minimum = Int.MinValue
  }

  class List_Ord[T](implicit ev: Ord.Common { type This = T }) extends Ord.Common { self =>
    type This = List[T]
    class Impl(`this`: This) extends Ord {
      val common: self.type = self
      def compareTo(that: List[T]): Int = (`this`, that) match {
        case (Nil, Nil) => 0
        case (Nil, _) => -1
        case (_, Nil) => +1
        case (x :: xs, y :: ys) =>
          val fst = ev.inject(x).compareTo(y)
          if (fst != 0) fst else List_Ord[T].inject(xs).compareTo(ys)
      }
    }
    def inject(x: This): Impl = new Impl(x)
    val minimum = Nil
  }

  implicit def List_Ord[T](implicit ev: Ord.Common { type This = T }): List_Ord[T] =
    new List_Ord[T]

  def max[T](x: T, y: T)(implicit ev: Ord.Common { type This = T }) = if (ev.inject(x) < y) x else y

  def max[T](xs: List[T])(implicit ev: Ord.Common { type This = T }): T =
    (Ord.by[T].minimum /: xs)(max(_, _))

  val x1 = max(1, 2)
  val x2 = max(List(1), Nil)
  val x3 = max(List(1, 2, 3))
  val x4 = max(List(List(1, 2), List(3, 3), Nil))

/* --------------------------------------------------------------------------

  Higher-kinded:

  trait Functor[A]
    def map[B](f: A => B): This[B]
  common
    type This[A]

  trait Monad[A] extends Functor[A]
    def flatMap[B](f: A => This[B]): This[B]
    def map[B](f: A => B): This[B] = flatMap(f `andThen` pure)
    def pure[A]: This[A]

  extension [A] for List[A] : Monad[A]
    def flatMap[B](f: A => List[B]): List[B] = this.flatMap(f)
  common
    def pure[A](x: A) = x :: Nil

  def g[F: Functor, A, B](x: A, f: A => B): Functor.by[F].This[B] =
    Functor.by[F].pure(x).map(f)

  def h[F: Monad, A, B](x: A): (A => Monad.by[F].This[B]) => Monad.by[F].This[B]) =
    f => Monad.by[F].pure(x).flatMap(f)

* ---------------------------------------------------------------------------- */

  trait Functor[A] {
    val common: Functor.Common
    import common._

    def map[B](f: A => B): This[B]
  }

  object Functor {
    trait Common { self =>
      type This[A]
      def inject[A](x: This[A]): Functor[A] { val common: self.type }
    }
    inline def by[F[_]](implicit ev: Functor.Common { type This[A] = F[A] }): ev.type = ev
  }

  trait Monad[A] extends Functor[A] { self =>
    val common: Monad.Common
    import common._

    def flatMap[B](f: A => This[B]): This[B]
    def map[B](f: A => B): This[B] = flatMap(f `andThen` pure)
  }
  object Monad {
    trait Common extends Functor.Common { self =>
      def pure[A](x: A): This[A]
      def inject[A](x: This[A]): Monad[A] { val common: self.type }
    }
    def by[F[_]](implicit ev: Monad.Common { type This[A] = F[A] }): ev.type = ev
  }

  implicit object List_Monad extends Monad.Common { self =>
    type This[A] = List[A]
    class Impl[A](`this`: This[A]) extends Monad[A] {
      val common: self.type = self
      def flatMap[B](f: A => This[B]): This[B] = `this`.flatMap(f)
    }
    def pure[A](x: A): This[A] = x :: Nil
    def inject[A](x: List[A]): Impl[A] = new Impl[A](x)
  }

  def g[F[_], A, B](x: A, f: A => B)(implicit ev: Monad.Common { type This[A] = F[A] })
    : ev.This[B] =
    ev.inject(Monad.by[F].pure(x)).map(f)

  def h[F[_], A, B](x: A)(implicit ev: Monad.Common { type This[A] = F[A] })
    : (A => ev.This[B]) => ev.This[B] =
    f => ev.inject(Monad.by[F].pure(x)).flatMap(f)

  val r = g[F = List](1, _.toString)

/* --------------------------------------------------------------------------

  Generic and Higher-kinded:

  extension [A, Ctx] for Ctx => A : Monad[A]
    def flatMap[B](f: A => Ctx => B): Ctx => B =
      ctx => f(this(ctx))(ctx)
  common
    def pure[A](x: A) = ctx => x

* ---------------------------------------------------------------------------- */

  class $eq$gt_Monad[Ctx] extends Monad.Common { self =>
    type This[A] = Ctx => A
    class Impl[A](`this`: This[A]) extends Monad[A] {
      val common: self.type = self
      def flatMap[B](f: A => This[B]): This[B] =
        ctx => f(`this`(ctx))(ctx)
    }
    def pure[A](x: A): This[A] =
      ctx => x
    def inject[A](x: This[A]): Impl[A] = new Impl[A](x)
  }

  implicit def $eq$gt_Monad[Ctx]: $eq$gt_Monad[Ctx] = new $eq$gt_Monad[Ctx]

  g[F = [X] =>> Int => X]((ctx: Int) => 1, x => (ctx: Int) => x.toString)


/* ---------------------------------------------------------------------------------

  case class Reactangle(width: Double, height: Double)
  case class Circle(radius: Double)

  implicit object RectangleArea
    def (x: Rectangle).area: Double = x.width * self.height

  trait HasArea[T]
    def (x: T).area: Double

  implicit object CircleHasArea extends HasArea[Circle]
    def (x: Circle).area = x.radius * 2 * math.Pi

 ---------------------------------------------------------------------------------

  trait SemiGroup[T]
    def (x: T).combine(y: T): T

  trait Monoid[T] extends SemiGroup[T]
    def unit: T

  def f[X: Monoid](x: X) = implicitly[Monoid[X]].unit.combine(x)

  def sum[T: Monoid](xs: List[T]): T =
    xs.foldLeft(implicitly[Monoid[T]].unit)(_ `combine` _)

  // Class `Str` is not definable

  implicit object StringMonoid extends Monoid[String]
    def (x: String).combine(y: String): String = x + y
    def unit = ""

  trait Ord[T]
    def (x: T).compareTo(y: T): Int
    def (x: T) < (that: T) = x.compareTo(y) < 0
    def (x: T) > (that: T) = x.compareTo(y) > 0
    val minimum: T
  }

  implicit object IntOrd {
    def (x: Int).compareTo(y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
  }

  implicit class ListOrd[T: Ord] {
    def (xs: List[T]).compareTo(ys: List[T]): Int = (xs, ys) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
    val minimum = Nil
  }

  def max[T: Ord](x: T, y: T) = if (x < y) y else x

  def max[T: Ord](xs: List[T]): T = implicitly[Ord[T]].minimum /: xs)(max(_, _))

 ---------------------------------------------------------------------------------

  trait Functor[F[_]]
    def (x: F[A]).map[A, B](f: A => B): F[B]

  trait Monad[F[_]] extends Functor[F]
    def (x: F[A]).flatMap[A, B](f: A => F[B]): F[B]
    def (x: F[A]).map[A, B](f: A => B) = x.flatMap(f `andThen` pure)
    def pure[A]: F[A]

  implicit object ListMonad extends Monad[List]
    def (xs: List[A]).flatMap[A, B](f: A => List[B]): List[B] = xs.flatMap(f)
    def pure[A]: List[A] = List.Nil

  implicit class ReaderMonad[Ctx] extends Monad[Ctx => _]
    def (r: Ctx => A).flatMap[A, B](f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A = ctx => x


*/

}