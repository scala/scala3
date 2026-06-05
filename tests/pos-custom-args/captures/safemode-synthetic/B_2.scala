import language.experimental.safe

def test(i: Int, j: (String, Int)) =
  // desugared to:
  // val a$1: List[String] = List.apply[String](["bar" : String]*)
  // val b$1: Option[String] @uncheckedVariance = A.f$default$2
  // A.f(a = a$1, b$1, c = 1000L)
  // We need to skip annotation check for the synthetic b$1 in safe mode
  A.f(a = List("bar"), c = 1000)
