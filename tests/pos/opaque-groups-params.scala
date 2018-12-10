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

  abstract class Wrapper[F[_]] { self =>
    def wraps[G[_], A](ga: G[A]): G[F[A]]
    def unwrap[G[_], A](gfa: G[F[A]]): G[A]

    final def apply[A](a: A): F[A] = wraps[Id, A](a)

    implicit object WrapperWrapping extends Wrapping[F] {
      def wraps[G[_], A](ga: G[A]): G[F[A]] = self.wraps(ga)
      def unwrap[G[_], A](ga: G[F[A]]): G[A] = self.unwrap(ga)
    }
  }

  // The following definition does not typecheck since the `First`
  // parent argument refers to the outer `First`, not the synthetic inner one.
  // See pos/opaque-groups.scala for a version that copmpiles.
  opaque type First[A] = A
  object First extends Wrapper[First] { // error: object creation impossible
    def wraps[G[_], A](ga: G[A]): G[First[A]] = ga // error: overriding
    def unwrap[G[_], A](gfa: G[First[A]]): G[A] = gfa
    implicit def firstSemigroup[A]: Semigroup[First[A]] =
      Semigroup.inst((x, y) => x)
  }

  opaque type Last[A] = A
  object Last extends Wrapper[Last] { // error: object creation impossible
    def wraps[G[_], A](ga: G[A]): G[Last[A]] = ga // error: overriding
    def unwrap[G[_], A](gfa: G[Last[A]]): G[A] = gfa
    implicit def lastSemigroup[A]: Semigroup[Last[A]] =
      Semigroup.inst((x, y) => y)
  }
}