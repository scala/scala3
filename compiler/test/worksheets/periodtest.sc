package dotty.tools.dotc.core

object periodtest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  import Periods._

  val p1 = Period(1, 2, 7)                        //> p1  : dotty.tools.dotc.core.Periods.Period = Period(2..7, run = 1)
  val p2 = Period(1, 3, 7)                        //> p2  : dotty.tools.dotc.core.Periods.Period = Period(3..7, run = 1)
  p1 contains p2                                  //> res0: Boolean = true
  p1 contains p1                                  //> res1: Boolean = true
  p2 contains p1                                  //> res2: Boolean = false
  p1 contains Period(0, 3, 3)                     //> res3: Boolean = false
  p1 contains Period(2, 3, 3)                     //> res4: Boolean = false
  p1 contains Nowhere                             //> res5: Boolean = false
  p1 overlaps p1                                  //> res6: Boolean = true
  Period(1, 2, 7) overlaps Period(1, 6, 9)        //> res7: Boolean = true
}