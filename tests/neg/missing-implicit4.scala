def testLocalInstance =
  trait Zip[F[_]]
  trait Traverse[F[_]] {
    extension [A, B, G[_] : Zip](fa: F[A]) def traverse(f: A => G[B]): G[F[B]]
  }

  object instances {
    given Zip[Option] as zipOption = ???
    given Traverse[List] as traverseList = ???
  }

  def ff(using xs: Zip[Option]) = ???

  ff // error

  List(1, 2, 3).traverse(x => Option(x)) // error

  locally {
    import instances.traverseList
    List(1, 2, 3).traverse(x => Option(x)) // error
  }
end testLocalInstance