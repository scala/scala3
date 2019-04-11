object K1 {
  class Foo[T]

  class Bar[F[_]]
  object Bar {
    implicit def barF[F[_]](implicit fooF: Foo[Bar[F]]): Bar[F] = null
  }

  class A[T]
  object A {
    implicit def fooA[F[_[_]]](implicit barB: F[B]): Foo[F[A]] = null
  }

  class B[T]
  object B {
    implicit def fooB[F[_[_]]]: Foo[F[B]] = null
  }
}

object K1U {
  class Foo[T]

  class Bar[F[_ <: Int]]
  object Bar {
    implicit def barF[F[_ <: Int]](implicit fooF: Foo[Bar[F]]): Bar[F] = null
  }

  class A[T <: Int]
  object A {
    implicit def fooA[F[_[_ <: Int]]](implicit barB: F[B]): Foo[F[A]] = null
  }

  class B[T <: Int]
  object B {
    implicit def fooB[F[_[_ <: Int]]]: Foo[F[B]] = null
  }
}

object K1L {
  class Foo[T]

  class Bar[F[_ >: Int]]
  object Bar {
    implicit def barF[F[_ >: Int]](implicit fooF: Foo[Bar[F]]): Bar[F] = null
  }

  class A[T >: Int]
  object A {
    implicit def fooA[F[_[_ >: Int]]](implicit barB: F[B]): Foo[F[A]] = null
  }

  class B[T >: Int]
  object B {
    implicit def fooB[F[_[_ >: Int]]]: Foo[F[B]] = null
  }
}

object K11 {
  class Foo[T]

  class Bar[F[_[_]]]
  object Bar {
    implicit def barF[F[_[_]]](implicit fooF: Foo[Bar[F]]): Bar[F] = null
  }

  class A[T[_]]
  object A {
    implicit def fooA[F[_[_[_]]]](implicit barB: F[B]): Foo[F[A]] = null
  }

  class B[T[_]]
  object B {
    implicit def fooB[F[_[_[_]]]]: Foo[F[B]] = null
  }
}

object K2 {
  class Foo[T]

  class Bar[F[_, _]]
  object Bar {
    implicit def barF[F[_, _]](implicit fooF: Foo[Bar[F]]): Bar[F] = null
  }

  class A[T, U]
  object A {
    implicit def fooA[F[_[_, _]]](implicit barB: F[B]): Foo[F[A]] = null
  }

  class B[T, U]
  object B {
    implicit def fooB[F[_[_, _]]]: Foo[F[B]] = null
  }
}

object Test {
  {
    import K1._
    implicitly[Bar[A]]
  }

  {
    import K1U._
    implicitly[Bar[A]]
  }

  {
    import K1L._
    implicitly[Bar[A]]
  }

  {
    import K11._
    implicitly[Bar[A]]
  }

  {
    import K2._
    implicitly[Bar[A]]
  }
}
