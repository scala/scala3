package dotty.tools.dotc.core

import Periods._

object test {
  val rid = 223                                   //> rid  : Int = 223
  val p = periodOf(rid, 2)                        //> p  : dotty.tools.dotc.core.Periods.Period = 228416
  val ivl = intervalOf(rid, 2, 4)                 //> ivl  : dotty.tools.dotc.core.Periods.Interval = 228482
  containsPeriod(ivl, p)                          //> res0: Boolean = true
  containsPeriod(ivl, periodOf(rid, 1))           //> res1: Boolean = false
  containsPeriod(ivl, periodOf(rid, 5))           //> res2: Boolean = false
	containsPeriod(ivl, periodOf(rid, 4))     //> res3: Boolean = true
	containsPeriod(ivl, periodOf(rid+1, 2))   //> res4: Boolean = false
	containsPeriod(ivl, periodOf(rid-1, 2))   //> res5: Boolean = false
  phaseIdOf(p)                                    //> res6: dotty.tools.dotc.core.Periods.PhaseId = 2
  phaseIdOf(ivl)                                  //> res7: dotty.tools.dotc.core.Periods.PhaseId = 4
  runIdOf(p)                                      //> res8: dotty.tools.dotc.core.Periods.RunId = 223
  containsPeriod(intervalOf(rid, 2, 2), p)        //> res9: Boolean = true
}