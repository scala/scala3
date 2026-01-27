//> using options -Wunused:all -Wconf:name=InlinedAnonClassWarning:s

import collection.mutable.{Map => MutMap} // warn
import collection.mutable.Set // warn

class A {
  import collection.mutable.{Map => MutMap} // OK
  private val a = 1 // warn
  val b = 2 // OK

  /* This goes around the trivial method detection */
  val default_int = 12

  val someMap = MutMap()

  private def c1 = 2 // warn
  private def c2 = 2 // OK
  def c3 = c2

  def d1(using x: Int): Int = default_int // warn param
  def d2(using x: Int): Int = x // OK
  def d3(using Int): Int = summon[Int] // OK
  def d4(using Int): Int = default_int // warn

  def e1(x: Int) = default_int // warn param
  def e2(x: Int) = x // OK
  def f =
    val x = 1 // warn
    def f = 2 // warn
    val y = 3 // OK
    def g = 4 // OK
    y + g

  def g(x: Int): Int = x match
    case x: 1 => 0 // no warn same name as selector (for shadowing or unused)
    case x: 2 => x // OK
    case _    => 1 // OK

  def h(x: Int): Int = x match
    case y: 1 => 0 // warn unused despite trivial type and RHS
    case y: 2 => y // OK
    case _    => 1 // OK
}

/* ---- CHECK scala.annotation.unused ---- */
package foo.test.scala.annotation:
  import annotation.unused // OK

  /* This goes around the trivial method detection */
  val default_int = 12

  def a1(a: Int) = a // OK
  def a2(a: Int) = default_int // warn

  def a3(@unused a: Int) = default_int //OK

  def b1 =
    def f = 1 // warn
    1

  def b2 =
    def f = 1 // OK
    f

  def b3 =
    @unused def f = 1 // OK
    1

  object Foo:
    private def a = 1 // warn
    private def b = 2 // OK
    @unused private def c = 3 // OK

    def other = b

package foo.test.companionprivate:
  class A:
    import A.b // OK
    def a = b // OK

  object A:
    private def b = c // OK
    def c = List(1,2,3) // OK

package foo.test.i16678:
  def foo(func: Int => String, value: Int): String = func(value) // OK

  def run =
    println(foo(number => number.toString, value = 5)) // OK
    println(foo(number => "<number>", value = 5)) // warn
    println(foo(func = number => number.toString, value = 5)) // OK
    println(foo(func = number => "<number>", value = 5)) // warn
    println(foo(func = _.toString, value = 5)) // OK

package foo.test.possibleclasses:
  case class AllCaseClass(
    k: Int, // OK
    private val y: Int // OK /* Kept as it can be taken from pattern */
  )(
    s: Int, // warn
    val t: Int, // OK
    private val z: Int // warn
  )

  case class AllCaseUsed(
    k: Int, // OK
    private val y: Int // OK
  )(
    s: Int, // OK
    val t: Int, // OK
    private val z: Int // OK
  ) {
    def a = k + y + s + t + z
  }

  class AllClass(
    k: Int, // warn
    private val y: Int // warn
  )(
    s: Int, // warn
    val t: Int, // OK
    private val z: Int // warn
  )

  class AllUsed(
    k: Int, // OK
    private val y: Int // OK
  )(
    s: Int, // OK
    val t: Int, // OK
    private val z: Int // OK
  ) {
    def a = k + y + s + t + z
  }

package foo.test.possibleclasses.withvar:
  case class AllCaseClass(
    k: Int, // OK
    private var y: Int // warn unset
  )(
    s: Int, // warn
    var ttt: Int, // OK
    private var zzz: Int // warn
  )

  case class AllCaseUsed(
    k: Int, // OK
    private var y: Int // warn unset
  )(
    s: Int, // OK
    var tt: Int, // OK global scope can be set somewhere else
    private var zz: Int // warn not set
  ) {
    def a = k + y + s + tt + zz
  }

  class AllClass(
    k: Int, // warn
    private var y: Int // warn
  )(
    s: Int, // warn
    var t: Int, // OK
    private var z: Int // warn
  )

  class AllUsed(
    k: Int, // OK
    private var y: Int // warn not set
  )(
    s: Int, // OK
    var t: Int, // OK global scope can be set somewhere else
    private var z: Int // warn not set
  ) {
    def a = k + y + s + t + z
  }



package foo.test.from.i16675:
  case class PositiveNumber private (i: Int) // OK
  object PositiveNumber:
    def make(i: Int): Option[PositiveNumber] = //OK
      Option.when(i >= 0)(PositiveNumber(i)) // OK

package foo.test.i16822:
  enum ExampleEnum {
    case Build(context: String) // OK
    case List // OK
  }

  def demo = {
    val x = ExampleEnum.List // OK
    println(x) // OK
  }

package foo.test.i16877:
  import scala.collection.immutable.HashMap // OK
  import scala.annotation.StaticAnnotation // OK

  class ExampleAnnotation(val a: Object) extends StaticAnnotation // OK

  @ExampleAnnotation(new HashMap()) // OK
  class Test //OK

package foo.test.i16926:
  def hello(): Unit =
    for {
      i <- (0 to 10).toList // warn patvar
      (a, b) = "hello" -> "world" // OK
    } yield println(s"$a $b")

package foo.test.i16925:
  def hello =
    for {
      i <- 1 to 2 if true // OK
      _ = println(i) // OK
    } yield ()

package foo.test.i16863a:
  import scala.quoted.*
  def fn(using Quotes) =
    val x = Expr(1)
    '{ $x + 2 } // OK

package foo.test.i16863b:
  import scala.quoted.*
  def fn[A](using Quotes, Type[A]) = // OK
    val numeric = Expr.summon[Numeric[A]].getOrElse(???)
    '{ $numeric.fromInt(3) } // OK

package foo.test.i16863c:
  import scala.quoted.*
  def fn[A](expr: Expr[Any])(using Quotes) =
    val imp = expr match
      case '{ ${ _ }: a } => Expr.summon[Numeric[a]] // OK
    println(imp)

package foo.test.i16863d:
  import scala.quoted.*
  import scala.compiletime.asMatchable // OK
  def fn[A](using Quotes, Type[A]) =
    import quotes.reflect.*
    val imp = TypeRepr.of[A].widen.asMatchable match
      case Refinement(_,_,_) => ()
    println(imp)

package foo.test.i16679a:
  object myPackage:
    trait CaseClassName[A]:
      def name: String
    object CaseClassName:
      trait CaseClassByStringName[A] extends CaseClassName[A]
      import scala.deriving.Mirror
      object CaseClassByStringName:
        inline final def derived[A](using inline A: Mirror.Of[A]): CaseClassByStringName[A] =
          new CaseClassByStringName[A]:
            def name: String = A.toString

  object secondPackage:
    import myPackage.CaseClassName // OK
    case class CoolClass(i: Int) derives CaseClassName.CaseClassByStringName
    println(summon[CaseClassName[CoolClass]].name)

package foo.test.i16679b:
  object myPackage:
    trait CaseClassName[A]:
      def name: String

    object CaseClassName:
      import scala.deriving.Mirror
      inline final def derived[A](using inline A: Mirror.Of[A]): CaseClassName[A] =
        new CaseClassName[A]:
          def name: String = A.toString

  object Foo:
    given x: myPackage.CaseClassName[secondPackage.CoolClass] = null

  object secondPackage:
    import myPackage.CaseClassName // warn
    import Foo.x
    case class CoolClass(i: Int)
    println(summon[myPackage.CaseClassName[CoolClass]])

package foo.test.i17156:
  package a:
    trait Foo[A]
    object Foo:
      inline def derived[T]: Foo[T] = new Foo {}

  package b:
    import a.Foo
    type Xd[A] = Foo[A]

  package c:
    import b.Xd
    trait Z derives Xd


package foo.test.i17175:
  val continue = true
  def foo =
    for {
      i <- 1.until(10) // OK
      if continue
    } {
      println(i)
    }

package foo.test.i17117:
  package example {
    object test1 {
      val test = "test"
    }

    object test2 {

      import example.test1 as t1

      val test = t1.test
    }
  }

// manual testing of cached look-ups
package deeply:
  object Deep:
    def f(): Unit =
      def g(): Unit =
        def h(): Unit =
          println(Deep)
          println(Deep)
          println(Deep)
        h()
      g()
    override def toString = "man, that is deep!"
/* result cache saves before context traversal and import/member look-up
CHK object Deep
recur * 10 = context depth is 10 between reference and definition
CHK method println
recur = was 19 at predef where max root is 21
CHK object Deep
recur = cached result
CHK method println
recur
CHK object Deep
recur
*/

package constructors:
  class C private (i: Int): // warn param
    def this() = this(27)
    private def this(s: String) = this(s.toInt) // warn ctor
    def c = new C(42)
    private def f(i: Int) = i // warn overloaded member
    private def f(s: String) = s
    def g = f("hello") // use one of overloaded member

  class D private (i: Int):
    private def this() = this(42) // no warn used by companion
    def d = i
  object D:
    def apply(): D = new D()

package reversed: // reverse-engineered
  class C:
    def c: scala.Int = 42 // Int marked used; lint does not look up <empty>.scala
  class D:
    def d: Int = 27 // Int is found in root import scala.*
  class E:
    import scala.* // no warn because root import (which is cached! by previous lookup) is lower precedence
    def e: Int = 27
