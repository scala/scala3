    object unsound {
      trait Bound[A, B <: A]
      trait Bind[A] {
        def bad[B <: A](bound: Bound[A, B], b: B) = b
      }
      def coerce[T, U](t: T): U = {
        lazy val bound: Bound[U, _ >: T] = ??? // error: >: T does not conform to upper bound
        def bind = new Bind[U] {}
        bind.bad(bound, t)
      }
    }
