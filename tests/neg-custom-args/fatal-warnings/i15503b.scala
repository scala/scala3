// scalac: -Wunused:locals

val a = 1 // OK

val b = // OK
  val e1 = 1 // error
  def e2 = 2 // error
  1

val c = // OK
  val e1 = 1 // OK
  def e2 = e1 // OK
    e2

def d = 1 // OK

def e = // OK
  val e1 = 1 // error
  def e2 = 2 // error
  1

def f = // OK
  val f1 = 1 // OK
  def f2 = f1 // OK
  f2

class Foo {
  val b = // OK
    val e1 = 1 // error
    def e2 = 2 // error
    1

  val c = // OK
    val e1 = 1 // OK
    def e2 = e1 // OK
      e2

  def d = 1 // OK

  def e = // OK
    val e1 = 1 // error
    def e2 = 2 // error
    1

  def f = // OK
    val f1 = 1 // OK
    def f2 = f1 // OK
    f2
}

// ---- SCALA 2 tests ----

package foo.scala2.tests:
  class Outer {
    class Inner
  }

  trait Locals {
    def f0 = {
      var x = 1 // error
      var y = 2 // OK
      y = 3
      y + y
    }
    def f1 = {
      val a = new Outer // OK
      val b = new Outer // error
      new a.Inner
    }
    def f2 = {
      var x = 100
      x
    }
  }

  object Types {
    def l1() = {
      object HiObject { def f = this } // error
      class Hi { // error
        def f1: Hi = new Hi
        def f2(x: Hi) = x
      }
      class DingDongDoobie // error
      class Bippy // OK
      type Something = Bippy // OK
      type OtherThing = String // error
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
