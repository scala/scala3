//> using options -deprecation -Wunused:nowarn

import scala.annotation._

class C {
  @deprecated("message", "1.2.3") def dep = 0
}

object sd884 {
  @nowarn("cat=deprecation") def t0 = new C().dep // no warn
  @nodep def t1 = new C().dep // no warn
  @purr def t2 = new C().dep  // warn // warn: unused @nowarn

  @nowarn("msg=pure expression does nothing") def tZ = { 1; 2 }  // no warn
  @purr def t3 = { 1; 2 }  // no warn
  @nodep def t4 = { 1; 2 } // warn // warn: unused @nowarn
}
