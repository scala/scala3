/** A blue sky sketch how one might evolve extensions to do type classes
 *  without the current gap between functional and OO patterns.
 *  Largely inspired by the way Rust does it.
 *
 *  The main additions (to be worked out in detail) is a self type `This` and
 *  a mechanism that a trait can abstract over companions of classes that implement it.
 *  Companion types and methods are declared using `object` for now, just to
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
    def add (that: This): This
  }

  trait Monoid extends SemiGroup {
    static def unit: This
  }

  extend Int : Monoid {
    def add (that: Int) = this + that
    static def unit = 0
  }

  extend String : Monoid {
    def add (that: Int) = this ++ that
    static def unit = ""
  }

  def sum[T: Monoid](xs: List[T]): T =
    (instance[T, Monoid].unit /: xs)(_ `add` _)

// --->
            {
              trait TypeClass {
                type This
                type Static[This]
              }

              trait Implementation[From, To <: TypeClass] {
                type This = From
                def statics: To # Static[From]
                def inject(x: From): To { type This = From }
              }

              trait SemiGroup extends TypeClass {
                def + (that: This): This
              }

              trait Monoid extends SemiGroup {
                class Static[This] { def unit: This }
              }

              implicit object extend_Int_Monoid extends Monoid#Static[Int] with Implementation[Int, Monoid] {
                def unit: Int = 0
                def inject($this: Int) = new Monoid {
                  type This = Int
                  def + (that: This): This = $this + that
                }
              }

              implicit object extend_String_Monoid extends Monoid#Static[String] with Implementation[String, Monoid] {
                def unit = ""
                def inject($this: String): Monoid { type This = String } =
                  new Monoid {
                    type This = String
                    def + (that: This): This = $this + that
                  }
              }

              def impl[From, To](implicit ev: Implementation[From, To]): Implementation[From, To] =
                ev

              def sum[T](xs: List[T])(implicit $ev: Implementation[T, Monoid]) = {
                //val ev = impl[T, Monoid]
                ($ev.statics.unit /: xs)((x, y) => $ev.inject(x) + y)
              }
            }

// Ord

  trait Ord {
    def compareTo(that: This): Int
    def < (that: This) = compareTo < 0
    def > (that: This) = compareTo > 0
  }

  extend List[type T : Ord] : Ord {
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
    def map[B](f: A => B): ThisC[B]
  }

  // Generically, `pure[A]{.map(f)}^n`
  def develop[A, F[X] : Functor[X]](n: Int, f: A => A): F[A] =
    if (n == 0) Functor.objects[F].pure[A]
    else develop[A, F](n - 1, f).map(f)

  trait Monad[A] extends Functor[A] {
    object type ThisC[A] <: Monad[A]

    def flatMap[B](f: A => ThisC[B]): ThisC[B]
    def map[B](f: A => B) = this.flatMap(f.andThen(pure))
  }

  extend List[type T] : Monad[T] {
    object type ThisC[A] = List[A]
    object def pure[A] = Nil

    def flatMap[B](f: A => List[B]): List[B] = this match {
      case x :: xs => f(x) ++ xs.flatMap(f)
      case Nil => Nil
    }
  }

  extend (type T[X]: Monad[X])[T[type A]] {
    def flatten: T[A] = this.flatMap(identity)
  }

// Iterables

  trait MonoIterable[A] {
    object type ThisC[A] <: MonoIterable[A]
    object def empty: This[A]
    object def apply(xs: A*): This[A]

    def filter(p: A => Boolean): This[A]
  }

  trait Iterable[A] extends MonoIterable[A] {
    object type ThisC[A] <: Iterable[A]

    def map[B](f: A => B): ThisC[B]
    def flatMap[B](f: A => ThisC[B]): ThisC[B]
  }

  extend String : MonoIterable[Char] {
    object type ThisC[A] = String
    object def empty = ""
    object def apply(xs: A*) = xs.mkString

    def filter(p: Char => Boolean): String = ...
    def map(f: Char => Char): String = ...
  }

  extend String : Iterable[Char] {
    object type ThisC[A] = IndexedSeq[A]

    def map[B](f: Char => B): IndexedSeq[B] = ...
    def flatMap[B](f: Char => IndexedSeq[B]): IndexedSeq[B] = ...
  }

  extend List[type T] : Iterable[T] {
    object type ThisC[A] = List[A]
    object def empty = Nil
    object def apply(xs: A*) = (xs /: Nil)(_ :: _)

    def filter(p: T => Boolean): List[T] = ...
    def map[B](f: T => B): List[B] = ...
    def flatMap[B](f: T => List[B]): List[B] = ...
  }
}