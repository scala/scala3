object Test extends App {

  implicit object O {
    extension (x: Int) def em: Boolean = x > 0
  }

  assert(1.em == O.em(1))

  case class Circle(x: Double, y: Double, radius: Double)

  extension (c: Circle) {
    def circumference: Double = c.radius * math.Pi * 2
  }

  val circle = new Circle(1, 1, 2.0)

  println(circle.circumference)

  given AnyRef {
    extension (xs: Seq[String]) def longestStrings: Seq[String] = {
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)
    }
  }
  val names = List("hi", "hello", "world")
  assert(names.longestStrings == List("hello", "world"))

  extension [T](xs: Seq[T]) {
    def second = xs.tail.head
  }

  assert(names.longestStrings.second == "world")

  extension [T](xs: List[List[T]]) {
    def flattened = xs.foldLeft[List[T]](Nil)(_ ++ _)
  }

  assert(List(names, List("!")).flattened == names :+ "!")
  assert(Nil.flattened == Nil)

  trait SemiGroup[T] {
    extension (x: T) def combine(y: T): T
  }
  trait Monoid[T] extends SemiGroup[T] {
    def unit: T
  }

  given Monoid[String] {
    extension (x: String) def combine(y: String): String = x.concat(y)
    def unit: String = ""
  }

  // Abstracting over a type class with a context bound:
  def sum[T: Monoid](xs: List[T]): T =
    xs.foldLeft(implicitly[Monoid[T]].unit)(_.combine(_))

  println(sum(names))

  trait Ord[T] {
    extension (x: T) def compareTo(y: T): Int
    extension (x: T) def < (y: T) = x.compareTo(y) < 0
    extension (x: T) def > (y: T) = x.compareTo(y) > 0
    val minimum: T
  }

  given Ord[Int] {
    extension (x: Int) def compareTo(y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
    val minimum = Int.MinValue
  }

  given [T: Ord] => Ord[List[T]] {
    extension (xs: List[T]) def compareTo(ys: List[T]): Int = (xs, ys).match {
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

  def max[T: Ord](xs: List[T]): T = (implicitly[Ord[T]].minimum /: xs)(max(_, _))

  println(max(List[Int]()))
  println(max(List(1, 2, 3)))

  println(max(List(1, 2, 3), List(2)))

  trait Functor[F[_]] {
    extension [A](x: F[A]) def map[B](f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    extension [A](x: F[A]) def flatMap[B](f: A => F[B]): F[B]
    extension [A](x: F[A]) def map[B](f: A => B) = x.flatMap(f `andThen` pure)

    def pure[A](x: A): F[A]
  }

  given Monad[List] {
    extension [A](xs: List[A]) def flatMap[B](f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)
  }

  given [Ctx] => Monad[[X] =>> Ctx => X] {
    extension [A](r: Ctx => A) def flatMap[B](f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x
  }

  def mappAll[F[_]: Monad, T](x: T, fs: List[T => T]): F[T] =
    fs.foldLeft(implicitly[Monad[F]].pure(x))((x: F[T], f: T => T) =>
      if (true) implicitly[Monad[F]].map(x)(f)
      else if (true) x.map(f)
      else x.map[T](f)
    )
}
