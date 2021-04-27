object testObjectInstance:
  trait Zip[F[_]]
  trait Traverse[F[_]] {
    extension [A, B, G[_] : Zip](fa: F[A]) def traverse(f: A => G[B]): G[F[B]]
  }

  object instances {
    given zipOption: Zip[Option] = ???
    given traverseList: Traverse[List] = ???
    extension [T](xs: List[T])
      def second: T = xs.tail.head
    extension [T](xs: List[T]) def first: T = xs.head
  }

  List(1, 2, 3).first // error
  List(1, 2, 3).second // error
  Array(1, 2, 3).first // error, no hint
end testObjectInstance
