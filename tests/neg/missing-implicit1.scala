object testObjectInstance:
  trait Zip[F[_]]
  trait Traverse[F[_]] {
    def [A, B, G[_] : Zip](fa: F[A]) traverse(f: A => G[B]): G[F[B]]
  }

  object instances {
    given zipOption as Zip[Option] = ???
    given traverseList as Traverse[List] = ???
    extension listExtension on [T](xs: List[T]):
      def second: T = xs.tail.head
    def [T](xs: List[T]) first: T = xs.head
  }

  def ff with (xs: Zip[Option]) = ???

  ff // error

  List(1, 2, 3).traverse(x => Option(x)) // error

  locally {
    import instances.traverseList
    List(1, 2, 3).traverse(x => Option(x)) // error
  }

  List(1, 2, 3).first // error
  List(1, 2, 3).second // error
  Array(1, 2, 3).first // error, no hint
end testObjectInstance

def testLocalInstance =
  trait Zip[F[_]]
  trait Traverse[F[_]] {
    def [A, B, G[_] : Zip](fa: F[A]) traverse(f: A => G[B]): G[F[B]]
  }

  object instances {
    given zipOption as Zip[Option] = ???
    given traverseList as Traverse[List] = ???
  }

  def ff with (xs: Zip[Option]) = ???

  ff // error

  List(1, 2, 3).traverse(x => Option(x)) // error

  locally {
    import instances.traverseList
    List(1, 2, 3).traverse(x => Option(x)) // error
  }
end testLocalInstance