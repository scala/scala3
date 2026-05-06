//> using options -Wunused:imports -Wconf:origin=Suppressed.*:s

object FooUnused:
  import collection.mutable.Set // warn
  import collection.mutable.{Map => MutMap} // warn
  import collection.mutable._ // warn

object FooWildcardUnused:
  import collection.mutable._ // warn

object Foo:
  import collection.mutable.Set // OK
  import collection.mutable.{Map => MutMap} // OK

  val bar = Set() // OK
  val baz = MutMap() // OK

object FooWildcard:
  import collection.mutable._ // OK

  val bar = Set() // OK

object FooNestedUnused:
  import collection.mutable.Set // warn
  object Nested:
    def hello = 1

object FooNested:
  import collection.mutable.Set // OK
  object Nested:
    def hello = Set()

object FooGivenUnused:
  import SomeGivenImports.given // warn

object FooGiven:
  import SomeGivenImports.given // OK
  import SomeGivenImports._ // warn

  val foo = summon[Int]

object SomeGivenImports:
  given Int = 0
  given String = "foo"

/**
 * Import used as type name are considered
 * as used.
 *
 * Import here are only used as types, not as
 * Term
 */
object FooTypeName:
  import collection.mutable.Set // OK
  import collection.mutable.Map // OK
  import collection.mutable.Seq // OK
  import collection.mutable.ArrayBuilder // OK
  import collection.mutable.ListBuffer // warn

  def checkImplicit[A](using Set[A]) = ()
  def checkParamType[B](a: Map[B,B]): Seq[B] = ???
  def checkTypeParam[A] = ()

  checkTypeParam[ArrayBuilder[Int]]


object InlineChecks:
  object InlineFoo:
    import collection.mutable.Set // ok
    import collection.mutable.Map // warn
    inline def getSet = Set(1)

  object InlinedBar:
    import collection.mutable.Set // warn (don't be fooled by inline expansion)
    import collection.mutable.Map // warn
    val a = InlineFoo.getSet      // expansion is attributed mutable.Set.apply(1)

object MacroChecks:
  object StringInterpol:
    import collection.mutable.Set // OK
    import collection.mutable.Map // OK
    println(s"This is a mutableSet : ${Set[Map[Int,Int]]()}")


object InnerMostCheck:
  import collection.mutable.* // warn
  def check =
    import collection.mutable.* //OK
    val a = Set(1)

object IgnoreExclusion:
  import collection.mutable.{Map => _, Set => _, *} // OK??
  import collection.mutable.{ListBuffer} // warn
  def check =
    val a = Set(1)
    val b = Map(1 -> 2)
    def c = Seq(42)

/* BEGIN : Check on packages*/
package nestedpackageimport:
  package p:
    class C
  package p:
    package q:
      import p.* // warn
      class U:
        def f = new C
package unnestedpackageimport:
  package p:
    class C
  package p.q:
    import p.* // nowarn
    class U:
      def f = new C

package redundancy:
  object redundant:
    def f = 42
  import redundancy.* // warn superseded by def in scope
  class R:
    def g = redundant.f

package testpackageimport:
  package a:
    val x: Int = 0

  package b:
    import a._ // warn


/* END : Check on packages*/

/* BEGIN : tests on meta-language features */
object TestGivenCoversionScala2:
  /* note: scala3 Conversion[U,T] do not require an import */
  import language.implicitConversions // OK

  implicit def doubleToInt(d:Double):Int = d.toInt

  def idInt(i:Int):Int = i
  val someInt = idInt(1.0)

object TestTailrecImport:
  import annotation.tailrec // OK
  @tailrec
  def fac(x:Int, acc:Int = 1): Int =
    if x == 0 then acc else fac(x - 1, acc * x)
/* END : tests on meta-language features */

/* BEGIN : tests on given import order */
object GivenImportOrderAtoB:
  class X
  class Y extends X
  object A { implicit val x: X = new X }
  object B { implicit val y: Y = new Y }
  class C {
    import A._ // warn
    import B._ // OK
    def t = implicitly[X]
  }

object GivenImportOrderBtoA:
  class X
  class Y extends X
  object A { implicit val x: X = new X }
  object B { implicit val y: Y = new Y }
  class C {
    import B._ // OK
    import A._ // warn
    def t = implicitly[X]
  }
/* END : tests on given import order */

/* Scala 2 implicits */
object Scala2ImplicitsGiven:
  object A:
    implicit val x: Int = 1
  object B:
    import A.given  // OK
    val b = summon[Int]
  object C:
    import A.given  // warn
    val b = 1
  object D:
    import A._  // OK
    val b = summon[Int]
  object E:
    import A._  // warn
    val b = 1
  object F:
    import A.x  // OK
    val b = summon[Int]
  object G:
    import A.x  // warn
    val b = 1

// -------------------------------------
object TestNewKeyword:
  object Foo:
    class Aa[T](val x: T)
  object Bar:
    import Foo.Aa // OK
    val v = 1
    val a = new Aa(v)

// -------------------------------------
object testAnnotatedType:
  import annotation.switch // OK
  val a = (??? : @switch) match
    case _ => ???


//-------------------------------------
package testImportsInImports:
  package a:
    package b:
      val x = 1
  package c:
    import a.b // OK
    import b.x // OK
    import b.x as z // OK
    val y = x + z

//-------------------------------------
package testOnOverloadedMethodsImports:
  package a:
    trait A
    trait B
    trait C:
      def foo(x: A):A = ???
      def foo(x: B):B = ???
  package b:
    object D extends a.C
  package c:
    import b.D.foo // warn
  package d:
    import b.D.foo // OK
    def bar = foo((??? : a.A))
  package e:
    import b.D.foo // OK
    def bar = foo((??? : a.B))
  package f:
    import b.D.foo // OK
    def bar = foo((??? : a.A))
    def baz = foo((??? : a.B))

//-------------------------------------
package foo.testing.rename.imports:
  import collection.mutable.{Set => MutSet1} // OK
  import collection.mutable.{Set => MutSet2} // OK
  import collection.mutable.{Set => MutSet3} // warn
  type A[X] = MutSet1[X]
  val a = MutSet2(1)

//-------------------------------------
package foo.testing.imports.precedence:
  import scala.collection.immutable.{BitSet => _, _} // warn
  import scala.collection.immutable.BitSet // OK
  def t = BitSet.empty

package foo.test.enums:
  enum A: // OK
    case B extends A // OK
    case C extends A // OK

package foo.test.typeapply.hklamdba.i16680:
  package foo:
    trait IO[A]

  package bar:
    import foo.IO // OK

    def f[F[_]]: String = "hello"
    def go = f[IO]

object Selections:
  def f(list: List[Int]): Int =
    import list.{head => first} // OK
    first

  def f2(list: List[Int]): Int =
    import list.head // OK
    head

  def f3(list: List[Int]): Int =
    import list.head // warn
    list.head

  object N:
    val ns: List[Int] = Nil

  def g(): Int =
    import N.ns // OK
    ns.head
end Selections

object `more nestings`:
  object Outer:
    object Inner:
      val thing = 42
      def j() =
        import Inner.thing // warn
        thing
      def k() =
        import Inner.thing // warn
        Inner.thing

  object Thing:
    object Inner:
      val thing = 42
      import Inner.thing // warn
      def j() =
        thing
      def k() =
        Inner.thing

object Suppressed:
  val suppressed = 42
object Suppressing:
  import Suppressed.* // no warn, see options
  def f = 42

package i22692:
  import javax.swing.*
  import javax.swing.event as swingEvent // no warn, regression test for warning in 3.6

  type b = AbstractButton
  type t = swingEvent.AncestorListener

package ancient:
  package p {
    class Bippo {
      def length: Int = 123
      class Tree
    }
  }

  package object p1 {
    import p._
    class A
    implicit class B(val s: String) { def bippy = s }
    val c: Bippo = new Bippo
    type D = String
  }
  package object p2 {
    import p._
    class A
    implicit class B(val s: String) { def bippy = s }
    val c: Bippo = new Bippo
    type D = Int
  }

  trait NoWarn {
    {
      import p1._ // no warn
      println("abc".bippy)
    }

    {
      import p1._ // no warn
      println(new A)
    }

    {
      import p1.B // no warn
      println("abc".bippy)
    }

    {
      import p1._ // no warn
      import c._  // no warn
      println(length)
    }

    {
      import p1._ // no warn
      import c._  // no warn
      val x: Tree = null
      println(x)
    }

    {
      import p1.D // no warn
      val x: D = null
      println(x)
    }
  }

  trait Warn {
    {
      import p1.A // warn
      println(123)
    }

    {
      import p1.{ A, B } // warn on A
      println("abc".bippy)
    }

    {
      import p1.{ A, B } //warn // warn on both
      println(123)
    }

    {
      import p1._ // no warn (technically this could warn, but not worth the effort to unroll unusedness transitively)
      import c._  // warn
      println(123)
    }

    {
      import p1._ // warn
      println(123)
    }

    {
      class Tree
      import p1._ // no warn
      import c._  // warn
      val x: Tree = null
      println(x)
    }

    {
      import p1.c._  // warn
      println(123)
    }
  }

  trait Nested {
    {
      import p1._   // warn
      trait Warn {
        import p2.*
        println(new A)
        println("abc".bippy)
      }
      println("")
    }

    {
      import p1._   // no warn
      trait NoWarn {
        import p2.B  // no warn
        println("abc".bippy)
        println(new A)
      }
      println(new NoWarn { })
    }

    {
      import p1.A   // warn
      trait Warn {
        import p2.A
        println(new A)
      }
      println(new Warn { })
    }
  }
end ancient

object `i22970 assign lhs was ignored`:
  object X:
    var global = 0
  object Main:
    import X.global // no warn
    def main(args: Array[String]): Unit =
      global = 1
