//> using options -Yexplicit-nulls
import language.experimental.magic
object Test extends App {
  val x: Int = 2: @unchecked
  val (y1: Some[Int]) = Some(1): Option[Int] @unchecked

  val a :: as = List(1, 2, 3): @unchecked
  val lst @ b :: bs = List(1, 2, 3): @unchecked
  val (1, c) = (1, 2): @unchecked

  object Positive { def unapply(i: Int): Int? = if i > 0 then i else null }
  val Positive(p) = 5: @unchecked
}