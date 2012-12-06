package dotty.tools.dotc.core

import Periods._

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(78); 
  val rid = 223;System.out.println("""rid  : Int = """ + $show(rid ));$skip(27); 
  val p = periodOf(rid, 2);System.out.println("""p  : dotty.tools.dotc.core.Periods.Period = """ + $show(p ));$skip(34); 
  val ivl = intervalOf(rid, 2, 4);System.out.println("""ivl  : dotty.tools.dotc.core.Periods.Interval = """ + $show(ivl ));$skip(25); val res$0 = 
  containsPeriod(ivl, p);System.out.println("""res0: Boolean = """ + $show(res$0));$skip(40); val res$1 = 
  containsPeriod(ivl, periodOf(rid, 1));System.out.println("""res1: Boolean = """ + $show(res$1));$skip(40); val res$2 = 
  containsPeriod(ivl, periodOf(rid, 5));System.out.println("""res2: Boolean = """ + $show(res$2));$skip(39); val res$3 = 
	containsPeriod(ivl, periodOf(rid, 4));System.out.println("""res3: Boolean = """ + $show(res$3));$skip(41); val res$4 = 
	containsPeriod(ivl, periodOf(rid+1, 2));System.out.println("""res4: Boolean = """ + $show(res$4));$skip(41); val res$5 = 
	containsPeriod(ivl, periodOf(rid-1, 2));System.out.println("""res5: Boolean = """ + $show(res$5));$skip(15); val res$6 = 
  phaseIdOf(p);System.out.println("""res6: dotty.tools.dotc.core.Periods.PhaseId = """ + $show(res$6));$skip(17); val res$7 = 
  phaseIdOf(ivl);System.out.println("""res7: dotty.tools.dotc.core.Periods.PhaseId = """ + $show(res$7));$skip(13); val res$8 = 
  runIdOf(p);System.out.println("""res8: dotty.tools.dotc.core.Periods.RunId = """ + $show(res$8));$skip(43); val res$9 = 
  containsPeriod(intervalOf(rid, 2, 2), p);System.out.println("""res9: Boolean = """ + $show(res$9))}
}
