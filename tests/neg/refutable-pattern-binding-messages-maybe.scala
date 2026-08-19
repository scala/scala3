//> using options -source 3.8 -Yexplicit-nulls
import language.experimental.magic
object Test {
  // refutable extractor
  object Positive { def unapply(i: Int): Int? = if i > 0 then i else null }
  val Positive(p) = 5                     // warn: refutable extractor
  for Positive(i) <- List(1, 2, 3) do ()  // error: refutable extractor

  // more specialized
  val xs: List[AnyRef] = ???
  val i :: is = List(1, 2, 3)             // warn: pattern type more specialized
  for ((x: String) <- xs) do ()           // error: pattern type more specialized

  // does not match
  val ys: List[Option[?]] = ???
  for none @ None <- ys do ()             // error: pattern type does not match
  val 1 = 2                               // warn: pattern type does not match
}
