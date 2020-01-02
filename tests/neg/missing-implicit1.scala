object testObjectInstance with
  trait Zip[F[_]]
  trait Traverse[F[_]] {
    def [A, B, G[_] : Zip](fa: F[A]) traverse(f: A => G[B]): G[F[B]]
  }

  object instances {
    given zipOption: Zip[Option] = ???
    given traverseList: Traverse[List] = ???
  }

  def ff(given xs: Zip[Option]) = ???

  //import instances.zipOption

  ff // error

  List(1, 2, 3).traverse(x => Option(x)) // error

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

  //import instances.zipOption

  ff // error

  List(1, 2, 3).traverse(x => Option(x)) // error
