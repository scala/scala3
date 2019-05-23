object Test {
  enum Hoge[F[_]] derives Eql {
    case A extends Hoge[List]
    case B extends Hoge[[X] =>> String]
  }
  import Hoge._

  A == A
  A == (B: Hoge[_])

  A == B // should be error: cannot be compared, needs proper typeclass drivation of `Eql` to get there.

  class C

  A == "" // error: cannot be compared
  A == new C // error: cannot be compared

}

object Test2 {
  enum Hoge[F[G[_]]] derives Eql {
    case A extends Hoge[[F[_]] =>> F[Int]]
    case B extends Hoge[[F[_]] =>> F[String]]
  }
  import Hoge._

  A == A
  A == (B: Hoge[_])

  A == B

  class C

  A == "" // error: cannot be compared
  A == new C // error: cannot be compared

}

object Test3 {
  enum Hoge[F[G[_]]] derives Eql {
    case A extends Hoge[[X] =>> List]   // error: wrong kind
    case B extends Hoge[[X] =>> [Y] =>> String]  // error: wrong kind
  }
  import Hoge._

  A == A
  A == (B: Hoge[_])

  A == B

  class C

  A == ""
  A == new C

}


