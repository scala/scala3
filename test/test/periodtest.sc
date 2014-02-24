package dotty.compiler.internal
package core

object periodtest {
  println("Welcome to the Scala worksheet")
  
  import Periods._
  
  val p1 = Period(1, 2, 7)
  val p2 = Period(1, 3, 7)
  p1 contains p2
  p1 contains p1
  p2 contains p1
  p1 contains Period(0, 3, 3)
  p1 contains Period(2, 3, 3)
  p1 contains Nowhere
  p1 overlaps p1
  Period(1, 2, 7) overlaps Period(1, 6, 9)
}