package org.scalajs.testsuite.compiler

/* Most of this file is a copy of `tests/run/structural.scala`, adapted to work
 * under JUnit for Scala.js.
 * There are more tests at the end of the file that are not in run/structural.scala.
 */

import scala.reflect.Selectable.reflectiveSelectable

import org.junit.Assert.*
import org.junit.Test

object ReflectiveCallTestScala3 {
  class C { type S = String; type I }
  class D extends C { type I = Int }

  type Foo = {
    def sel0: Int
    def sel1: Int => Int
    def fun0(x: Int): Int

    def fun1(x: Int)(y: Int): Int
    def fun2(x: Int): Int => Int
    def fun3(a1: Int, a2: Int, a3: Int)
            (a4: Int, a5: Int, a6: Int)
            (a7: Int, a8: Int, a9: Int): Int

    def fun4(implicit x: Int): Int
    def fun5(x: Int)(implicit y: Int): Int

    def fun6(x: C, y: x.S): Int
    def fun7(x: C, y: x.I): Int
    def fun8(y: C): y.S
    def fun9(y: C): y.I
  }

  class Foo1 {
    def sel0: Int = 1
    def sel1: Int => Int = x => x
    def fun0(x: Int): Int = x

    def fun1(x: Int)(y: Int): Int = x + y
    def fun2(x: Int): Int => Int = y => x * y
    def fun3(a1: Int, a2: Int, a3: Int)
            (a4: Int, a5: Int, a6: Int)
            (a7: Int, a8: Int, a9: Int): Int = -1

    def fun4(implicit x: Int): Int = x
    def fun5(x: Int)(implicit y: Int): Int = x + y

    def fun6(x: C, y: x.S): Int = 1
    def fun7(x: C, y: x.I): Int = 2
    def fun8(y: C): y.S = "Hello"
    def fun9(y: C): y.I = 1.asInstanceOf[y.I]
  }

  class Foo2 extends scala.Selectable {
    def sel0: Int = 1
    def sel1: Int => Int = x => x
    def fun0(x: Int): Int = x

    def fun1(x: Int)(y: Int): Int = x + y
    def fun2(x: Int): Int => Int = y => x * y
    def fun3(a1: Int, a2: Int, a3: Int)
            (a4: Int, a5: Int, a6: Int)
            (a7: Int, a8: Int, a9: Int): Int = -1

    def fun4(implicit x: Int): Int = x
    def fun5(x: Int)(implicit y: Int): Int = x + y

    def fun6(x: C, y: x.S): Int = 1
    def fun7(x: C, y: x.I): Int = 2
    def fun8(y: C): y.S = "Hello"
    def fun9(y: C): y.I = 1.asInstanceOf[y.I]
  }

  def basic(x: Foo): Unit ={
    assert(x.sel0 == 1)
    assert(x.sel1(2) == 2)
    assert(x.fun0(3) == 3)

    val f = x.sel1
    assert(f(3) == 3)
  }

  def currying(x: Foo): Unit = {
    assert(x.fun1(1)(2) == 3)
    assert(x.fun2(1)(2) == 2)
    assert(x.fun3(1, 2, 3)(4, 5, 6)(7, 8, 9) == -1)
  }

  def etaExpansion(x: Foo): Unit = {
    val f0 = x.fun0(_)
    assert(f0(2) == 2)

    val f1 = x.fun0 _
    assert(f1(2) == 2)

    val f2 = x.fun1(1)(_)
    assert(f2(2) == 3)

    val f3 = x.fun1(1) _
    assert(f3(2) == 3)

    val f4 = x.fun1(1)
    assert(f4(2) == 3)
  }

  def implicits(x: Foo) = {
    implicit val y = 2
    assert(x.fun4 == 2)
    assert(x.fun5(1) == 3)
  }

  // Limited support for dependant methods
  def dependent(x: Foo) = {
    val y = new D

    assert(x.fun6(y, "Hello") == 1)
    // assert(x.fun7(y, 1) == 2) // error: No ClassTag available for x.I

    val s = x.fun8(y)
    assert((s: String) == "Hello")

    // val i = x.fun9(y) // error: rejected (blows up in pickler if not rejected)
    // assert((i: String) == "Hello") // error: Type mismatch: found: y.S(i); required: String
  }
}

class ReflectiveCallTestScala3 {
  import ReflectiveCallTestScala3.*

  @Test def testBasic1(): Unit = basic(new Foo1)
  @Test def testCurrying1(): Unit = currying(new Foo1)
  @Test def testEtaExpansion1(): Unit = etaExpansion(new Foo1)
  @Test def testImplicits1(): Unit = implicits(new Foo1)
  @Test def testDependent1(): Unit = dependent(new Foo1)

  @Test def testBasic2(): Unit = basic(new Foo2)
  @Test def testCurrying2(): Unit = currying(new Foo2)
  @Test def testEtaExpansion2(): Unit = etaExpansion(new Foo2)
  @Test def testImplicits2(): Unit = implicits(new Foo2)
  @Test def testDependent2(): Unit = dependent(new Foo2)

  @Test def testAllSpecialTypes(): Unit = {
    // Test types that have special ClassTags
    type Foo = {
      def foo(unit: Unit, any: Any, anyVal: AnyVal, anyRef: AnyRef, obj: Object): String

      def nullMeth(nul: Null): Any
      def nothingMeth(nothing: Nothing): Any
    }

    class FooImpl {
      def foo(unit: Unit, any: Any, anyVal: AnyVal, anyRef: AnyRef, obj: Object): String =
        s"$unit $any $anyVal $anyRef $obj"

      def nullMeth(nul: Null): Any = "" + nul
      def nothingMeth(nothing: Nothing): Any = ???
    }

    val foo: Foo = new FooImpl
    assertEquals("undefined any-string 5 Some(5) None",
        foo.foo((), "any-string", 5, Some[Int](5), None))

    assertEquals("null", foo.nullMeth(null))

    // Make sure that a call to nothingMeth can link
    if (Math.random() > 2) // always false
      foo.nothingMeth(???)
  }

}
