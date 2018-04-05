/** A blue sky sketch how one might evolve extensions to do type classes
 *  without the current gap between functional and OO patterns.
 *  Largely inspired by the way Rust does it.
 *
 *  The main additions (to be worked out in detail) is a self type `This` and
 *  a mechanism that a trait can abstract over companions of classes that implement it.
 *  Companion types and methods are declared using `static` for now, just to
 *  pick some familiar notation.
 *
 *  Ideas about `This` (still vague at the moment)
 *
 *   - Treat it as an additional abstract type in a trait, prefixed by the name of the trait
 *   - An implementing (non-trait) class binds the `This` types of all the traits it implements
 *     to itself.
 *   - Later subclasses do not re-bind `This` types already bound by their superclasses.
 *     (so in that sense, `This`-binding is like trait parameterization, the first implementing
 *     classes determines the self type and the parameters of a trait)
 *   - Paramerized classes have parameterized `This` types (e.g. Functor below).
 */
import Predef.{any2stringadd => _, _}
object blueSkyExtensions {

// Semigroup and Monoid

  trait SemiGroup {
    def + (that: This): This
  }

  trait Monoid extends SemiGroup {
    static def unit: This
  }

  extension IntMonoid for Int : Monoid {
    static def unit = 0
    def + (that: Int) = this + that
  }

  extension StringMonoid for String : Monoid {
    static def unit = ""
    def + (that: Int) = this ++ that
  }

  def sum[T: Monoid](xs: List[T]): T =
    (instance[T, Monoid].unit /: xs)(_ `add` _)

// Ord

  trait Ord {
    def compareTo(that: This): Int
    def < (that: This) = compareTo < 0
    def > (that: This) = compareTo > 0
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

// Functor and Monad

  trait Functor[A] {
    static def pure[A]: This[A]
    def map[B](f: A => B): This[B]
  }

  // Generically, `pure[A]{.map(f)}^n`
  def develop[A, F[X] : Functor[X]](n: Int, f: A => A): F[A] =
    if (n == 0) Functor.statics[F].pure[A]
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

// Iterables

  trait MonoIterable[A] {
    static def empty: This[A]
    static def apply(xs: A*): This[A]

    def filter(p: A => Boolean): This[A]
  }

  trait Iterable[A] extends MonoIterable[A] {
    def map[B](f: A => B): This[B]
    def flatMap[B](f: A => This[B]): This[B]
  }

  extension StringMonoIterable for String : MonoIterable[Char] {
    static type This[A] = String
    static def empty = ""
    static def apply(xs: A*) = xs.mkString

    def filter(p: Char => Boolean): String = ...
    def map(f: Char => Char): String = ...
  }

  extension StringIterable for String : Iterable[Char] {
    static type This[A] = IndexedSeq[A]

    def map[B](f: Char => B): IndexedSeq[B] = ...
    def flatMap[B](f: Char => IndexedSeq[B]): IndexedSeq[B] = ...
  }

  extension ListIterable[T] for List[T] : Iterable[T] {
    static type This[A] = List[A]
    static def empty = Nil
    static def apply(xs: A*) = (xs /: Nil)(_ :: _)

    def filter(p: T => Boolean): List[T] = ...
    def map[B](f: T => B): List[B] = ...
    def flatMap[B](f: T => List[B]): List[B] = ...
  }
}
