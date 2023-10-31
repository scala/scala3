object wildcardInInfixType {

  val useless: ? => ? = (x: Int) => 1

  val pointless: (? <: Int) => ? = (x: Int) => 1

  val answer: Int Either ? = Left(42)
}

