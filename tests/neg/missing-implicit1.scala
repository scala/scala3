object testObjectInstance with
  trait Zip[F[_]]
  trait Traverse[F[_]] {
    def [A, B, G[_] : Zip](fa: F[A]) traverse(f: A => G[B]): G[F[B]]
  }

  object instances {
    given zipOption: Zip[Option] = ???
    given traverseList: Traverse[List] = ???
    given listExtension: [T](xs: List[T]) extended with
      def second: T = xs.tail.head
    def [T](xs: List[T]) first: T = xs.head
  }

  def ff(given xs: Zip[Option]) = ???

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
    given zipOption: Zip[Option] = ???
    given traverseList: Traverse[List] = ???
  }

  def ff(given xs: Zip[Option]) = ???

  ff // error

  List(1, 2, 3).traverse(x => Option(x)) // error

  locally {
    import instances.traverseList
    List(1, 2, 3).traverse(x => Option(x)) // error
  }
end testLocalInstance