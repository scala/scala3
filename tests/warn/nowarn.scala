//> using options -deprecation -Wunused:nowarn

// derived by tests/warn/nowarnRangePos.scala in scala/scala
// adding "slash slash warn" comments

import scala.annotation._

class ann(a: Any) extends Annotation

class C {
  @deprecated("message", "1.2.3") def dep = 0

  @nowarn def t0 = { 0: @nowarn; 1 }     // warn: outer @nowarn unused
  @nowarn def t1 = { 0: Int @nowarn; 1 } // warn: inner @nowarn unused, it covers the type, not the expression
  @nowarn @ann(dep) def t2 = 0          // warn: deprecation warning, @nowarn unused
//@ann(dep: @nowarn) def t3 = 0         // silent (should be)

  @nowarn("cat=deprecation") def t4 = dep

  def t5 = (new I1a).m

  // completion forced by method above
  @nowarn class I1a { // warn: unused @nowarn
    @nowarn def m = { 1; 2 }
  }

  // completion during type checking
  @nowarn class I1b { // warn: unused @nowarn
    @nowarn def m = { 1; 2 }
  }

  def t6 = (new I1b).m

  @nowarn val t7a = { 0; 1 }
  val t7b = { 0; 1 } // warn: discard pure

  @nowarn class I2a {
    def f: Unit = 1
  }
  class I2b {
    def f: Unit = 1 // warn: discard non-Unit
  }

  trait I3a
  @nowarn object I3a { // warn: supresses nothing
    def main(args: Array[String]) = () // the warning is only present in Scala 2 backend (see I3b)
  }
  trait I3b
  object I3b {
    def main(args: Array[String]) = () // main method in companion of trait triggers a warning in the backend
  }

  def t8(): Unit = {
    @nowarn
    val a = {
      123
      ()
    }
    val b = {
      123 // warn: discard pure
      ()
    }
  }

  @nowarn("msg=Discarded non-Unit value")
  def t9a(): Unit = {
    123
  }
  @nowarn("msg=something else") // warn: unused
  def t9b(): Unit = {
    123 // warn: discard non-Unit
  }

  @nowarn
  def t10a(): Unit = {
    123
  }
  def t10b(): Unit = {
    123 // warn: discard non-Unit
  }

  def t11(): Unit = {
    val a = dep: @nowarn
    a + dep // warn: deprecated
  }

  @nowarn object T12 {
    @nowarn("v") def f = try 1 // warn: try without catch/finally
    def g = { 1; 2 }
  }

  @nowarn("verbose") object T13 {
    @nowarn def f = try 1
    def g = { 1; 2 } // warn: discard pure
    @nowarn("v") def unused = 0 // warn: unused @nowarn
  }
}

trait T {
  @nowarn val t1 = { 0; 1 }
}

class K extends T

// ### unknown filter: site
//@nowarn("site=Uh.f.g") // would-be-warn: ?
//class Uh {
//  def f = {
//    def g(c: C) = c.dep // would-be-warn: ?
//  }
//}

object sd884 {
  class nodep extends nowarn("cat=deprecation")
  class purr extends nowarn("msg=pure expression does nothing")

  @nowarn("cat=deprecation") def t0 = new C().dep // no warn
  @nodep def t1 = new C().dep // no warn
  @purr def t2 = new C().dep  // warn // warn: unused @nowarn

  @nowarn("msg=pure expression does nothing") def tZ = { 1; 2 }  // no warn
  @purr def t3 = { 1; 2 }  // no warn
  @nodep def t4 = { 1; 2 } // warn // warn: unused @nowarn
}
