//> using options  -Wunused:locals

val a = 1 // OK

var cs = 3 // OK

val b = // OK
  var e3 = 2 // warn
  val e1 = 1 // warn
  def e2 = 2 // warn
  1

val c = // OK
  var e1 = 1 // warn not set
  def e2 = e1 // OK
  val e3 = e2 // OK
    e3

val g = // OK
  var e1 = 1 // OK
  def e2 = e1 // OK
  val e3 = e2 // OK
  e1 = e3 // OK
    e3

def d = 1 // OK

def e = // OK
  val e1 = 1 // warn
  def e2 = 2 // warn
  var e3 = 4 // warn
  1

def f = // OK
  val f1 = 1 // OK
  var f2 = f1 // warn not set
  def f3 = f2 // OK
  f3

def h = // OK
  val f1 = 1 // OK
  var f2 = f1 // OK
  def f3 = f2 // OK
  f2 = f3 // OK
  f2

class Foo {
  val a = 1 // OK

  var cs = 3 // OK

  val b = // OK
    var e3 = 2 // warn
    val e1 = 1 // warn
    def e2 = 2 // warn
    1

  val c = // OK
    var e1 = 1 // warn not set
    def e2 = e1 // OK
    val e3 = e2 // OK
    e3

  val g = // OK
    var e1 = 1 // OK
    def e2 = e1 // OK
    val e3 = e2 // OK
    e1 = e3 // OK
    e3

  def d = 1 // OK

  def e = // OK
    val e1 = 1 // warn
    def e2 = 2 // warn
    var e3 = 4 // warn
    1

  def f = // OK
    val f1 = 1 // OK
    var f2 = f1 // warn not set
    def f3 = f2 // OK
    f3

  def h = // OK
    val f1 = 1 // OK
    var f2 = f1 // OK
    def f3 = f2 // OK
    f2 = f3 // OK
    f2
}

// ---- SCALA 2 tests ----

package foo.scala2.tests:
  class Outer {
    class Inner
  }

  trait Locals {
    def f0 = {
      var x = 1 // warn
      var y = 2 // OK
      y = 3
      y + y
    }
    def f1 = {
      val a = new Outer // OK
      val b = new Outer // warn
      new a.Inner
    }
    def f2 = {
      var x = 100 // warn not set
      x
    }
  }

  object Types {
    def l1() = {
      object HiObject { def f = this } // warn
      class Hi { // warn
        def f1: Hi = new Hi
        def f2(x: Hi) = x
      }
      class DingDongDoobie // warn
      class Bippy // OK
      type Something = Bippy // OK
      type OtherThing = String // warn
      (new Bippy): Something
    }
  }

package test.foo.twisted.i16682:
  def myPackage =
    object IntExtractor: // OK
      def unapply(s: String): Option[Int] = s.toIntOption

    def isInt(s: String) = s match { // OK
      case IntExtractor(i) => println(s"Number $i")
      case _ => println("NaN")
    }
    isInt

  def f = myPackage("42")
