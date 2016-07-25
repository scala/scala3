object wildcardInInfixType {

  val useless: _ => _ = (x: Int) => 1

  val pointless: (_ <: Int) => _ = (x: Int) => 1

  val answer: Int Either _ = Left(42)
}

