object Test extends App {

  implicit object O {
    def em(this x: Int): Boolean = x > 0
  }

  assert(1.em == O.em(1))

  case class Circle(x: Double, y: Double, radius: Double)

  witness {
    def circumference(this c: Circle): Double = c.radius * math.Pi * 2
  }

  val circle = new Circle(1, 1, 2.0)

  println(circle.circumference)

  witness {
    def longestStrings(this xs: Seq[String]): Seq[String] = {
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)
    }
  }
  val names = List("hi", "hello", "world")
  assert(names.longestStrings == List("hello", "world"))

  witness {
    def second[T](this xs: Seq[T]) = xs.tail.head
  }

  assert(names.longestStrings.second == "world")

  witness {
    def flattened[T](this xs: List[List[T]]) = xs.foldLeft[List[T]](Nil)(_ ++ _)
  }

  assert(List(names, List("!")).flattened == names :+ "!")
  assert(Nil.flattened == Nil)

  trait SemiGroup[T] {
    def combine(this x: T)(y: T): T
  }
  trait Monoid[T] extends SemiGroup[T] {
    def unit: T
  }

  // An instance declaration:
  witness for Monoid[String] {
    def combine(this x: String)(y: String): String = x.concat(y)
    def unit: String = ""
  }

  // Abstracting over a typeclass with a context bound:
  def sum[T: Monoid](xs: List[T]): T =
    xs.foldLeft(implicitly[Monoid[T]].unit)(_.combine(_))

  println(sum(names))

  trait Ord[T] {
    def compareTo(this x: T)(y: T): Int
    def < (this x: T)(y: T) = x.compareTo(y) < 0
    def > (this x: T)(y: T) = x.compareTo(y) > 0
    val minimum: T
  }

  witness for Ord[Int] {
    def compareTo(this x: Int)(y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
    val minimum = Int.MinValue
  }

  witness [T: Ord] for Ord[List[T]] {
    def compareTo(this xs: List[T])(ys: List[T]): Int = (xs, ys) match {
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
    def map[A, B](this x: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](this x: F[A])(f: A => F[B]): F[B]
    def map[A, B](this x: F[A])(f: A => B) = x.flatMap(f `andThen` pure)

    def pure[A](x: A): F[A]
  }

  witness for Monad[List] {
    def flatMap[A, B](this xs: List[A])(f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)
  }

  witness [Ctx] for Monad[[X] => Ctx => X] {
    def flatMap[A, B](this r: Ctx => A)(f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x
  }

  def mappAll[F[_]: Monad, T](x: T, fs: List[T => T]): F[T] =
    fs.foldLeft(implicitly[Monad[F]].pure(x))((x: F[T], f: T => T) =>
      if (true) implicitly[Monad[F]].map(x)(f)
      else if (true) x.map(f)
      else x.map[T, T](f)
    )
}