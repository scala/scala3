@main def Test: Unit =
  println("case erased: " + testExpr { def erasedfn1(erased x: String) = "placeholder"; erasedfn1("arg1")})
  println("case erased nested: " + testExpr {
    def erasedfn2(p: String, erased q: String)(r: String, erased s: String) = p
    erasedfn2("a", "b")("c", "d")
  })
  println("case erased nested 2: " + testExpr {
    def erasedfn2(p: String, erased q: String)(erased r: String, s: String) = p
    erasedfn2("a", "b")("c", "d")
  })
