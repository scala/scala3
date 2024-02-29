//> using options -Xfatal-warnings -Wunused:all -source:3.3
//
class Bippy(a: Int, b: Int) {
  private def this(c: Int) = this(c, c)           // warn /Dotty:NoWarn
  private def boop(x: Int)            = x+a+b     // error
    private def bippy(x: Int): Int      = bippy(x)  // error TODO: could warn
  final private val MILLIS1           = 2000      // error no warn, /Dotty:Warn
  final private val MILLIS2: Int      = 1000      // error
  final private val HI_COMPANION: Int = 500       // no warn, accessed from companion
  def hi() = Bippy.HI_INSTANCE
}
object Bippy {
  def hi(x: Bippy) = x.HI_COMPANION
  private val HI_INSTANCE: Int = 500      // no warn, accessed from instance
  private val HEY_INSTANCE: Int = 1000    // error warn
  private lazy val BOOL: Boolean = true   // error warn
}

class A(val msg: String)
class B1(msg: String) extends A(msg)
class B2(msg0: String) extends A(msg0)
class B3(msg0: String) extends A("msg") // error /Dotty: unused explicit parameter

trait Bing

trait Accessors {
  private var v1: Int = 0 // error warn
  private var v2: Int = 0 // error warn, never set
  private var v3: Int = 0
  private var v4: Int = 0 // no warn

  private[this] var v5 = 0 // error warn, never set
  private[this] var v6 = 0
  private[this] var v7 = 0 // no warn

  def bippy(): Int = {
    v3 = 3
    v4 = 4
    v6 = 6
    v7 = 7
    v2 + v4 + v5 + v7
  }
}

class StableAccessors {
  private var s1: Int = 0 // error warn
  private var s2: Int = 0 // error warn, never set
  private var s3: Int = 0 // warn, never got /Dotty: no warn even if not usued
  private var s4: Int = 0 // no warn

  private[this] var s5 = 0 // error warn, never set
  private[this] var s6 = 0 // no warn, limitation /Dotty: Why limitation ?
  private[this] var s7 = 0 // no warn

  def bippy(): Int = {
    s3 = 3
    s4 = 4
    s6 = 6
    s7 = 7
    s2 + s4 + s5 + s7
  }
}

trait DefaultArgs {
  // warn about default getters for x2 and x3
  private def bippy(x1: Int, x2: Int = 10, x3: Int = 15): Int = x1 + x2 + x3 // no more warn warn since #17061

  def boppy() = bippy(5, 100, 200)
}


class Outer {
  class Inner
}

trait Locals {
  def f0 = {
    var x = 1 // error warn
    var y = 2
    y = 3
    y + y
  }
  def f1 = {
    val a = new Outer // no warn
    val b = new Outer // error warn
    new a.Inner
  }
  def f2 = {
    var x = 100 // error warn about it being a var, var not set
    x
  }
}

object Types {
  private object Dongo { def f = this } // no more warn since #17061
  private class Bar1 // error warn
  private class Bar2 // no warn
  private type Alias1 = String // error warn
  private type Alias2 = String // no warn
  def bippo = (new Bar2).toString

  def f(x: Alias2) = x.length

  def l1() = {
    object HiObject { def f = this } // no more warn since #17061
    class Hi { // error warn
      def f1: Hi = new Hi
      def f2(x: Hi) = x
    }
    class DingDongDoobie // error warn
    class Bippy // no warn
    type Something = Bippy // no warn
    type OtherThing = String // error warn
    (new Bippy): Something
  }
}

trait Underwarn {
  def f(): Seq[Int]

  def g() = {
    val Seq(_, _) = f()  // no warn
    true
  }
}

class OtherNames {
  private def x_=(i: Int): Unit = () // no more warn since #17061
  private def x: Int = 42 // error Dotty triggers unused private member : To investigate
  private def y_=(i: Int): Unit = () // // no more warn since #17061
  private def y: Int = 42

  def f = y
}


trait Forever {
  def f = {
    val t = Option((17, 42))
    for {
      ns <- t
      (i, j) = ns                        // no warn
    } yield (i + j)
  }
  def g = {
    val t = Option((17, 42))
    for {
      ns <- t
      (i, j) = ns                        // no warn
    } yield 42                           // val emitted only if needed, hence nothing unused
  }
}

trait Ignorance {
  private val readResolve = 42      // error ignore /dotty triggers unused private member/ why should we ignore ?
}

trait CaseyKasem {
  def f = 42 match {
    case x if x < 25 => "no warn"
    case y if toString.nonEmpty => "no warn" + y
    case z => "warn"
  }
}
trait CaseyAtTheBat {
  def f = Option(42) match {
    case Some(x) if x < 25 => "no warn"
    case Some(y @ _) if toString.nonEmpty => "no warn"
    case Some(z) => "warn"
    case None => "no warn"
  }
}

class `not even using companion privates`

object `not even using companion privates` {
  private implicit class `for your eyes only`(i: Int) {  // no more warn since #17061
    def f = i
  }
}

class `no warn in patmat anonfun isDefinedAt` {
  def f(pf: PartialFunction[String, Int]) = pf("42")
  def g = f {
    case s => s.length        // no warn (used to warn case s => true in isDefinedAt)
  }
}

// this is the ordinary case, as AnyRef is an alias of Object
class `nonprivate alias is enclosing` {
  class C
  type C2 = C
  private class D extends C2   // error warn
}

object `classof something` {
  private class intrinsically
  def f = classOf[intrinsically].toString()
}

trait `short comings` {
  def f: Int = {
    val x = 42 // error /Dotty only triggers in dotty
    17
  }
}

