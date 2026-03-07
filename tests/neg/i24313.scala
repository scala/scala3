type S[N <: Int]

type Foo[Xs] = Xs match
  case _ => S[String] // error
