object Test extends App {

  implicit object O {
    def (x: Int) em: Boolean = x > 0
  }

  assert(1.em == O.em(1))

  case class Circle(x: Double, y: Double, radius: Double)

  implied {
    def (c: Circle) circumference: Double = c.radius * math.Pi * 2
  }

  val circle = new Circle(1, 1, 2.0)

  println(circle.circumference)

  implied {
    def (xs: Seq[String]) longestStrings: Seq[String] = {
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)
    }
  }
  val names = List("hi", "hello", "world")
  assert(names.longestStrings == List("hello", "world"))

  implied {
    def (xs: Seq[T]) second[T] = xs.tail.head
  }

  assert(names.longestStrings.second == "world")

  implied {
    def (xs: List[List[T]]) flattened[T] = xs.foldLeft[List[T]](Nil)(_ ++ _)
  }

  assert(List(names, List("!")).flattened == names :+ "!")
  assert(Nil.flattened == Nil)

  trait SemiGroup[T] {
    def (x: T) combine (y: T): T
  }
  trait Monoid[T] extends SemiGroup[T] {
    def unit: T
  }

  // An implied declaration:
  implied for Monoid[String] {
    def (x: String) combine (y: String): String = x.concat(y)
    def unit: String = ""
  }

  // Abstracting over a typeclass with a context bound:
  def sum[T: Monoid](xs: List[T]): T =
    xs.foldLeft(implicitly[Monoid[T]].unit)(_.combine(_))

  println(sum(names))

  trait Ord[T] {
    def (x: T) compareTo (y: T): Int
    def (x: T) < (y: T) = x.compareTo(y) < 0
    def (x: T) > (y: T) = x.compareTo(y) > 0
    val minimum: T
  }

  implied for Ord[Int] {
    def (x: Int) compareTo (y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
    val minimum = Int.MinValue
  }

  implied [T: Ord] for Ord[List[T]] {
    def (xs: List[T]) compareTo (ys: List[T]): Int = (xs, ys) match {
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
    def (x: F[A]) map[A, B] (f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def (x: F[A]) flatMap[A, B] (f: A => F[B]): F[B]
    def (x: F[A]) map[A, B] (f: A => B) = x.flatMap(f `andThen` pure)

    def pure[A](x: A): F[A]
  }

  implied for Monad[List] {
    def (xs: List[A]) flatMap[A, B] (f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)
  }

  implied [Ctx] for Monad[[X] =>> Ctx => X] {
    def (r: Ctx => A) flatMap[A, B] (f: A => Ctx => B): Ctx => B =
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
