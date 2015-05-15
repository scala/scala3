object UnapplySeqTest {
  def unapplySeq(any: Any): Option[(Int, Seq[Int])] = Some((5, List(1)))
}

object Test extends dotty.runtime.LegacyApp {
  null match {
    case UnapplySeqTest(5) => println("uh-oh")
    case UnapplySeqTest(5, 1) => println("Matched!") // compiles
    case UnapplySeqTest(5, xs : _*) => println("toooo long: "+ (xs: Seq[Int]))
  }
}
