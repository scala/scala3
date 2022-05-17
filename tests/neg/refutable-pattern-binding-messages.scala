// scalac: -Werror
object Test {
  // refutable extractor
  object Positive { def unapply(i: Int): Option[Int] = Some(i).filter(_ > 0) }
  val Positive(p) = 5                     // error: refutable extractor
  for Positive(i) <- List(1, 2, 3) do ()  // error: refutable extractor

  // more specialized
  val xs: List[AnyRef] = ???
  val i :: is = List(1, 2, 3)             // error: pattern type more specialized
  for ((x: String) <- xs) do ()           // error: pattern type more specialized

  // does not match
  val ys: List[Option[?]] = ???
  for none @ None <- ys do ()             // error: pattern type does not match
  val 1 = 2                               // error: pattern type does not match
}
