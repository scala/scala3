sealed trait P
case class PC1(a: String) extends P
case class PC2(b: Int) extends P

@main def Test =
  println(MatchTest.test(PC1("ab"): P))
  println(MatchTest.test(PC2(10): P))
  println(MatchTest.test(null: P))
