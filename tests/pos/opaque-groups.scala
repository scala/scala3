package object groups {
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def inst[A](f: (A, A) => A): Semigroup[A] =
      new Semigroup[A] {
        def combine(x: A, y: A): A = f(x, y)
      }
  }

  type Id[A] = A

  trait Wrapping[F[_]] {
    def wraps[G[_], A](ga: G[A]): G[F[A]]
    def unwrap[G[_], A](ga: G[F[A]]): G[A]
  }

  abstract class Wrapper { self =>
    type F[_]
    def wraps[G[_], A](ga: G[A]): G[F[A]]
    def unwrap[G[_], A](gfa: G[F[A]]): G[A]

    final def apply[A](a: A): F[A] = wraps[Id, A](a)

    implicit object WrapperWrapping extends Wrapping[F] {
      def wraps[G[_], A](ga: G[A]): G[F[A]] = self.wraps(ga)
      def unwrap[G[_], A](ga: G[F[A]]): G[A] = self.unwrap(ga)
    }
  }

  opaque type First[A] = A
  object First extends Wrapper {
    type F[A] = First[A]
    def wraps[G[_], A](ga: G[A]): G[First[A]] = ga
    def unwrap[G[_], A](gfa: G[First[A]]): G[A] = gfa
    implicit def firstSemigroup[A]: Semigroup[First[A]] =
      Semigroup.inst((x, y) => x)
  }

  opaque type Last[A] = A
  object Last extends Wrapper {
    type F[A] = Last[A]
    def wraps[G[_], A](ga: G[A]): G[Last[A]] = ga
    def unwrap[G[_], A](gfa: G[Last[A]]): G[A] = gfa
    implicit def lastSemigroup[A]: Semigroup[Last[A]] =
      Semigroup.inst((x, y) => y)
  }

  opaque type Min[A] = A
  object Min extends Wrapper {
    type F[A] = Min[A]
    def wraps[G[_], A](ga: G[A]): G[Min[A]] = ga
    def unwrap[G[_], A](gfa: G[Min[A]]): G[A] = gfa
    implicit def minSemigroup[A](implicit o: Ordering[A]): Semigroup[Min[A]] =
      Semigroup.inst(o.min)
  }

  opaque type Max[A] = A
  object Max extends Wrapper {
    type F[A] = Max[A]
    def wraps[G[_], A](ga: G[A]): G[Max[A]] = ga
    def unwrap[G[_], A](gfa: G[Max[A]]): G[A] = gfa
    implicit def maxSemigroup[A](implicit o: Ordering[A]): Semigroup[Max[A]] =
      Semigroup.inst(o.max)
  }

  opaque type Plus[A] = A
  object Plus extends Wrapper {
    type F[A] = Plus[A]
    def wraps[G[_], A](ga: G[A]): G[Plus[A]] = ga
    def unwrap[G[_], A](gfa: G[Plus[A]]): G[A] = gfa
    implicit def plusSemigroup[A](implicit n: Numeric[A]): Semigroup[Plus[A]] =
      Semigroup.inst(n.plus)
  }

  opaque type Times[A] = A
  object Times extends Wrapper {
    type F[A] = Times[A]
    def wraps[G[_], A](ga: G[A]): G[Times[A]] = ga
    def unwrap[G[_], A](gfa: G[Times[A]]): G[A] = gfa
    implicit def timesSemigroup[A](implicit n: Numeric[A]): Semigroup[Times[A]] =
      Semigroup.inst(n.times)
  }

  opaque type Reversed[A] = A
  object Reversed extends Wrapper {
    type F[A] = Reversed[A]
    def wraps[G[_], A](ga: G[A]): G[Reversed[A]] = ga
    def unwrap[G[_], A](gfa: G[Reversed[A]]): G[A] = gfa
    implicit def reversedOrdering[A](implicit o: Ordering[A]): Ordering[Reversed[A]] =
      o.reverse
  }

  opaque type Unordered[A] = A
  object Unordered extends Wrapper {
    type F[A] = Unordered[A]
    def wraps[G[_], A](ga: G[A]): G[Unordered[A]] = ga
    def unwrap[G[_], A](gfa: G[Unordered[A]]): G[A] = gfa
    implicit def unorderedOrdering[A]: Ordering[Unordered[A]] =
      Ordering.by(_ => ())
  }
}