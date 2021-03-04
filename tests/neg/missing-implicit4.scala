def testLocalInstance =
  trait Zip[F[_]]
  trait Traverse[F[_]] {
    extension [A, B, G[_]](fa: F[A]) def traverse(f: A => G[B])(using Zip[G]): G[F[B]]
  }

  object instances {
    given zipOption: Zip[Option] = ???
    given traverseList: Traverse[List] = ???
  }

  def ff(using xs: Zip[Option]) = ???

  ff // error

  List(1, 2, 3).traverse(x => Option(x)) // error

  locally {
    import instances.traverseList
    List(1, 2, 3).traverse(x => Option(x)) // error
  }
end testLocalInstance