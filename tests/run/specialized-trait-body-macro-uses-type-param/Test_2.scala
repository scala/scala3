//> using options -language:experimental.specializedTraits

class B extends A[Int]
class C extends A[String]

@main def Test =
  assert(B().name == "scala.Int", B().name)
  assert(C().name == "scala.Predef.String", C().name)
