object Test extends App {

  def (x: Int) em: Boolean = x > 0

  assert(1.em == em(1))

  case class Circle(x: Double, y: Double, radius: Double)

  def circumference(this: Circle): Double = this.radius * math.Pi * 2
  def circumference2(this: Circle): Double = radius * math.Pi * 2

  val circle = new Circle(1, 1, 2.0)

  assert(circle.circumference == circumference(circle))

  def longestStrings(this: Seq[String]): Seq[String] = {
    val maxLength = map(_.length).max
    filter(_.length == maxLength)
  }
  val names = List("hi", "hello", "world")
  assert(names.longestStrings == List("hello", "world"))

  def second[T](this: Seq[T]) = tail.head

  assert(names.longestStrings.second == "world")

  def flattened[T](this: List[List[T]]) = foldLeft[List[T]](Nil)(_ ++ _)

  assert(List(names, List("!")).flattened == names :+ "!")
  assert(Nil.flattened == Nil)

  trait SemiGroup[T] {
    def combine(this: T)(that: T): T
  }
  trait Monoid[T] extends SemiGroup[T] {
    def unit: T
  }

  // An instance declaration:
  delegate StringMonoid for Monoid[String] {
    def combine(this: String)(that: String): String = this.concat(that)
    def unit: String = ""
  }

  // Abstracting over a typeclass with a context bound:
  def sum[T: Monoid](xs: List[T]): T =
    xs.foldLeft(implicitly[Monoid[T]].unit)(_.combine(_))

  println(sum(names))

  trait Ord[T] {
    def compareTo(this: T)(that: T): Int
    def < (this: T)(that: T) = this.compareTo(that) < 0
    def > (this: T)(that: T) = this.compareTo(that) > 0
    val minimum: T
  }

  delegate IntOrd for Ord[Int] {
    def compareTo(this: Int)(that: Int) =
      if (this < that) -1 else if (this > that) +1 else 0
    val minimum = Int.MinValue
  }

  delegate ListOrd[T: Ord] for Ord[List[T]] {
    def compareTo(this: List[T])(that: List[T]): Int = (this, that) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
    }
    val minimum: List[T] = Nil
  }

  def max[T: Ord](x: T, y: T): T = if (x < y) y else x

  def max[T: Ord](xs: List[T]): T = (the[Ord[T]].minimum /: xs)(max(_, _))

  println(max(List[Int]()))
  println(max(List(1, 2, 3)))

  println(max(List(1, 2, 3), List(2)))

  trait Functor[F[_]] {
    def map [A, B](this: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](this: F[A])(f: A => F[B]): F[B]
    def map [A, B](this: F[A])(f: A => B) = this.flatMap(f `andThen` pure)
    def pure[A](x: A): F[A]
  }

  implicit object ListMonad extends Monad[List] {
    def flatMap[A, B](this: List[A])(f: A => List[B]): List[B] =
      this.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)
  }

  class ReaderMonad[Ctx] extends Monad[[X] =>> Ctx => X] {
    def flatMap[A, B](this: Ctx => A)(f: A => Ctx => B): Ctx => B =
      ctx => f(this(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x
  }
  implicit def readerMonad[Ctx]: ReaderMonad[Ctx] = new ReaderMonad[Ctx]

  def mappAll[F[_]: Monad, T](x: T, fs: List[T => T]): F[T] =
    fs.foldLeft(implicitly[Monad[F]].pure(x))((x: F[T], f: T => T) =>
      if (true) implicitly[Monad[F]].map(x)(f)
      else if (true) x.map(f)
      else x.map[T, T](f)
    )
}