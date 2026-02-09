//
//> using options -deprecation -Wunused:privates,locals
//
class Bippy(a: Int, b: Int) {
  private def this(c: Int) = this(c, c)           // warn (DO warn, was NO)
  private def bippy(x: Int): Int      = bippy(x)  // warn
  private def boop(x: Int)            = x+a+b     // warn
  final private val MILLIS1           = 2000      // warn, scala2: no warn, might have been inlined
  final private val MILLIS2: Int      = 1000      // warn
  final private val HI_COMPANION: Int = 500       // no warn, accessed from companion
  def hi() = Bippy.HI_INSTANCE
}
object Bippy {
  def hi(x: Bippy) = x.HI_COMPANION
  private val HI_INSTANCE: Int = 500      // no warn, accessed from instance
  private val HEY_INSTANCE: Int = 1000    // warn
  private lazy val BOOL: Boolean = true   // warn
}

class A(val msg: String)
class B1(msg: String) extends A(msg)
class B2(msg0: String) extends A(msg0)
class B3(msg0: String) extends A("msg")

trait Accessors {
  private var v1: Int = 0 // warn
  private var v2: Int = 0 // warn, never set
  private var v3: Int = 0 // warn, never got
  private var v4: Int = 0 // no warn

  private var v5 = 0 // warn, never set
  private var v6 = 0 // warn, never got
  private var v7 = 0 // no warn

  def bippy(): Int = {
    v3 = 3
    v4 = 4
    v6 = 6
    v7 = 7
    v2 + v4 + v5 + v7
  }
}

class StableAccessors {
  private var s1: Int = 0 // warn
  private var s2: Int = 0 // warn, never set
  private var s3: Int = 0 // warn, never got
  private var s4: Int = 0 // no warn

  private var s5 = 0 // warn, never set
  private var s6 = 0 // warn, never got
  private var s7 = 0 // no warn

  def bippy(): Int = {
    s3 = 3
    s4 = 4
    s6 = 6
    s7 = 7
    s2 + s4 + s5 + s7
  }
}

trait DefaultArgs {
  // DO warn about default getters for x2 and x3
  private def bippy(x1: Int, x2: Int = 10, x3: Int = 15): Int = x1 + x2 + x3 // warn // warn

  def boppy() = bippy(5, 100, 200)
}

/* scala/bug#7707 Both usages warn default arg because using PrivateRyan.apply, not new.
case class PrivateRyan private (ryan: Int = 42) { def f = PrivateRyan() }
object PrivateRyan { def f = PrivateRyan() }
*/

class Outer {
  class Inner
}

trait Locals {
  def f0 = {
    var x = 1 // warn
    var y = 2
    y = 3
    y + y
  }
  def f1 = {
    val a = new Outer // no warn
    val b = new Outer // warn
    new a.Inner
  }
  def f2 = {
    var x = 100 // warn about it being a var
    x
  }
}

object Types {
  private object Dongo { def f = this } // warn
  private class Bar1 // warn
  private class Bar2 // no warn
  private type Alias1 = String // warn
  private type Alias2 = String // no warn
  def bippo = (new Bar2).toString

  def f(x: Alias2) = x.length

  def l1() = {
    object HiObject { def f = this } // warn
    class Hi { // warn
      def f1: Hi = new Hi
      def f2(x: Hi) = x
    }
    class DingDongDoobie // warn
    class Bippy // no warn
    type Something = Bippy // no warn
    type OtherThing = String // warn
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
  private def x_=(i: Int): Unit = () // warn
  private def x: Int = 42 // warn
  private def y_=(i: Int): Unit = () // warn
  private def y: Int = 42

  def f = y
}

case class C(a: Int, b: String, c: Option[String])
case class D(a: Int)

// patvars which used to warn as vals in older scala 2
trait Boundings {

  def c = C(42, "hello", Some("world"))
  def d = D(42)

  def f() = {
    val C(x, y, Some(z)) = c: @unchecked              // no warn
    17
  }
  def g() = {
    val C(x @ _, y @ _, Some(z @ _)) = c: @unchecked  // no warn
    17
  }
  def h() = {
    val C(x @ _, y @ _, z @ Some(_)) = c: @unchecked  // no warn for z?
    17
  }

  def v() = {
    val D(x) = d                          // no warn
    17
  }
  def w() = {
    val D(x @ _) = d                      // no warn
    17
  }

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
  private val readResolve = 42      // warn wrong signatured for special members
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
  private implicit class `for your eyes only`(i: Int) {  // warn
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
  private class D extends C2   // warn
}

object `classof something` {
  private class intrinsically
  def f = classOf[intrinsically].toString()
}

trait `scala 2 short comings` {
  def f: Int = {
    val x = 42 // warn
    17
  }
}

class `issue 12600 ignore abstract types` {
  type Abs
}

class `t12992 enclosing def is unused` {
  private val n = 42
  @annotation.unused def f() = n + 2 // unused code uses n
}

class `recursive reference is not a usage` {
  private def f(i: Int): Int = // warn
    if (i <= 0) i
    else f(i-1)
  private class P { // warn
    def f() = new P()
  }
}

class `absolve serial framework` extends Serializable:
  import java.io.{IOException, ObjectInputStream, ObjectOutputStream, ObjectStreamException}
  @throws(classOf[IOException])
  private def writeObject(stream: ObjectOutputStream): Unit = ()
  @throws(classOf[ObjectStreamException])
  private def writeReplace(): Object = ???
  @throws(classOf[ClassNotFoundException])
  @throws(classOf[IOException])
  private def readObject(stream: ObjectInputStream): Unit = ()
  @throws(classOf[ObjectStreamException])
  private def readObjectNoData(): Unit = ()
  @throws(classOf[ObjectStreamException])
  private def readResolve(): Object = ???

class `absolve ONLY serial framework`:
  import java.io.{IOException, ObjectInputStream, ObjectOutputStream, ObjectStreamException}
  @throws(classOf[IOException])
  private def writeObject(stream: ObjectOutputStream): Unit = () // warn
  @throws(classOf[ObjectStreamException])
  private def writeReplace(): Object = new Object // warn
  @throws(classOf[ClassNotFoundException])
  @throws(classOf[IOException])
  private def readObject(stream: ObjectInputStream): Unit = () // warn
  @throws(classOf[ObjectStreamException])
  private def readObjectNoData(): Unit = () // warn
  @throws(classOf[ObjectStreamException])
  private def readResolve(): Object = new Object // warn

@throws(classOf[java.io.ObjectStreamException])
private def readResolve(): Object = ??? // TODO warn
private def print() = println() // TODO warn
private val printed = false // TODO warn

package locked:
  private[locked] def locker(): Unit = () // TODO warn as we cannot distinguish unqualified private at top level
  package basement:
    private[locked] def unlock(): Unit = () // no warn as it is not top level at boundary

object `i19998 refinement`:
  trait Foo {
    type X[a]
  }
  trait Bar[X[_]] {
    private final type SelfX[a] = X[a] // was false positive
    val foo: Foo { type X[a] = SelfX[a] }
  }

object `patvar is assignable`:
  private var (i, j) = (42, 27) // no warn patvars under -Wunused:privates
  println((i, j))
