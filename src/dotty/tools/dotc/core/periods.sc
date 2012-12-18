package dotty.tools.dotc.core

import Periods._

object periods {
  val rid = 223
  val p = periodOf(rid, 2)
  val ivl = intervalOf(rid, 2, 4)
  containsPeriod(ivl, p)
  containsPeriod(ivl, periodOf(rid, 1))
  containsPeriod(ivl, periodOf(rid, 5))
	containsPeriod(ivl, periodOf(rid, 4))
	containsPeriod(ivl, periodOf(rid+1, 2))
	containsPeriod(ivl, periodOf(rid-1, 2))
  phaseIdOf(p)
  phaseIdOf(ivl)
  runIdOf(p)
  containsPeriod(intervalOf(rid, 2, 2), p)
}