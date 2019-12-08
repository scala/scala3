def foo234(x: Long): String = "new"

// Giving the compiler something to do so that we won't get
// the same modification date as A_1.scala
import math.Ordering

val y1 = {
  val x1 = summon[Ordering[Int]]
  val x2 = summon[Ordering[(Int, Int)]]
  val x3 = summon[Ordering[(Int, Int, Int)]]
  val x4 = summon[Ordering[(Int, Int, Int, Int)]]
  val x5 = summon[Ordering[(Int, Int, Int, Int, Int)]]
  val x6 = summon[Ordering[(Int, Int, Int, Int, Int, Int)]]
  val x7 = summon[Ordering[(Int, Int, Int, Int, Int, Int, Int)]]
  val x8 = summon[Ordering[(Int, Int, Int, Int, Int, Int, Int, Int)]]
}

val y2 = {
  val x1 = summon[Ordering[Int]]
  val x2 = summon[Ordering[(Int, Int)]]
  val x3 = summon[Ordering[(Int, Int, Int)]]
  val x4 = summon[Ordering[(Int, Int, Int, Int)]]
  val x5 = summon[Ordering[(Int, Int, Int, Int, Int)]]
  val x6 = summon[Ordering[(Int, Int, Int, Int, Int, Int)]]
  val x7 = summon[Ordering[(Int, Int, Int, Int, Int, Int, Int)]]
  val x8 = summon[Ordering[(Int, Int, Int, Int, Int, Int, Int, Int)]]
}

val y3 = {
  val x1 = summon[Ordering[Int]]
  val x2 = summon[Ordering[(Int, Int)]]
  val x3 = summon[Ordering[(Int, Int, Int)]]
  val x4 = summon[Ordering[(Int, Int, Int, Int)]]
  val x5 = summon[Ordering[(Int, Int, Int, Int, Int)]]
  val x6 = summon[Ordering[(Int, Int, Int, Int, Int, Int)]]
  val x7 = summon[Ordering[(Int, Int, Int, Int, Int, Int, Int)]]
  val x8 = summon[Ordering[(Int, Int, Int, Int, Int, Int, Int, Int)]]
}