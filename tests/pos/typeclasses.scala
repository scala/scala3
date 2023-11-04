class Common:

  // this should go in Predef
  infix type at [A <: { type This}, B] = A { type This = B }

  trait Ord:
    type This
    extension (x: This)
      def compareTo(y: This): Int
      def < (y: This): Boolean = compareTo(y) < 0
      def > (y: This): Boolean = compareTo(y) > 0

  trait SemiGroup:
    type This
    extension (x: This) def combine(y: This): This

  trait Monoid extends SemiGroup:
    def unit: This

  trait Functor:
    type This[A]
    extension [A](x: This[A]) def map[B](f: A => B): This[B]

  trait Monad extends Functor:
    def pure[A](x: A): This[A]
    extension [A](x: This[A])
      def flatMap[B](f: A => This[B]): This[B]
      def map[B](f: A => B) = x.flatMap(f `andThen` pure)
end Common


object Instances extends Common:

/*
  instance Int: Ord as intOrd with
    extension (x: Int)
      def compareTo(y: Int) =
        if x < y then -1
        else if x > y then +1
        else 0
*/
  given intOrd: Ord with
    type This = Int
    extension (x: Int)
      def compareTo(y: Int) =
        if x < y then -1
        else if x > y then +1
        else 0
/*
  instance List[T: Ord]: Ord as listOrd with
    extension (xs: List[T]) def compareTo(ys: List[T]): Int = (xs, ys) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
*/

  // Proposed short syntax:
  // given listOrd[T: Ord as ord]: Ord at T with
  given listOrd[T](using ord: Ord { type This = T}): Ord with
    type This = List[T]
    extension (xs: List[T]) def compareTo(ys: List[T]): Int = (xs, ys) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
  end listOrd

/*
  instance List: Monad as listMonad with
    extension [A](xs: List[A]) def flatMap[B](f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)
*/

  given listMonad: Monad with
    type This[A] = List[A]
    extension [A](xs: List[A]) def flatMap[B](f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)

/*
  type Reader[Ctx] = X =>> Ctx => X
  instance Reader[Ctx: _]: Monad as readerMonad with
    extension [A](r: Ctx => A) def flatMap[B](f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x
*/

  given readerMonad[Ctx]: Monad with
    type This[X] = Ctx => X
    extension [A](r: Ctx => A) def flatMap[B](f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x

  extension (xs: Seq[String])
    def longestStrings: Seq[String] =
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)

  extension [T](xs: List[T])
    def second = xs.tail.head
    def third = xs.tail.tail.head

  //Proposed short syntax:
  //extension [M: Monad as m, A](xss: M[M[A]])
  //  def flatten: M[A] =
  //    xs.flatMap(identity)

  extension [M, A](using m: Monad)(xss: m.This[m.This[A]])
    def flatten: m.This[A] =
      xss.flatMap(identity)

  // Proposed short syntax:
  //def maximum[T: Ord](xs: List[T]: T =
  def maximum[T](xs: List[T])(using Ord at T): T =
    xs.reduceLeft((x, y) => if (x < y) y else x)

  // Proposed short syntax:
  // def descending[T: Ord as asc]: Ord at T = new Ord:
  def descending[T](using asc: Ord at T): Ord at T = new Ord:
    type This = T
    extension (x: T) def compareTo(y: T) = asc.compareTo(y)(x)

  // Proposed short syntax:
  // def minimum[T: Ord](xs: List[T]) =
  def minimum[T](xs: List[T])(using Ord at T) =
    maximum(xs)(using descending)

  def test(): Unit =
    val xs = List(1, 2, 3)
    println(maximum(xs))
    println(maximum(xs)(using descending))
    println(maximum(xs)(using descending(using intOrd)))
    println(minimum(xs))

// Adapted from the Rust by Example book: https://doc.rust-lang.org/rust-by-example/trait.html
//
//           lines  words  chars
// wc Scala: 30     115     853
// wc Rust : 57     193    1466
trait Animal:
  type This
  // Associated function signature; `This` refers to the implementor type.
  def apply(name: String): This

  // Method signatures; these will return a string.
  extension (self: This)
    def name: String
    def noise: String
    def talk(): Unit = println(s"$name, $noise")
end Animal

class Sheep(val name: String):
  var isNaked = false
  def shear() =
    if isNaked then
      println(s"$name is already naked...")
    else
      println(s"$name gets a haircut!")
      isNaked = true

/*
instance Sheep: Animal with
  def apply(name: String) = Sheep(name)
  extension (self: This)
    def name: String = self.name
    def noise: String = if self.isNaked then "baaaaah?" else "baaaaah!"
    override def talk(): Unit =
      println(s"$name pauses briefly... $noise")
*/

// Implement the `Animal` trait for `Sheep`.
given Animal with
  type This = Sheep
  def apply(name: String) = Sheep(name)
  extension (self: This)
    def name: String = self.name
    def noise: String = if self.isNaked then "baaaaah?" else "baaaaah!"
    override def talk(): Unit =
      println(s"$name pauses briefly... $noise")

/*

 - In a type pattern, A <: T, A >: T, A: T, A: _ are all allowed and mean
   T is a fresh type variable (T can start with a capital letter).
 - instance definitions
 - `as m` syntax in context bounds and instance definitions

*/
