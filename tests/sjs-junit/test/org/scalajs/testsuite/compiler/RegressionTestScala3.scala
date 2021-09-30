package org.scalajs.testsuite.compiler

import org.junit.Assert.*
import org.junit.Test

import scala.scalajs.js
import scala.scalajs.js.annotation._

class RegressionTestScala3 {
  import RegressionTestScala3.*

  @Test def testRegressionDoubleDefinitionOfOuterPointerIssue10177(): Unit = {
    assertEquals(6, new OuterClassIssue10177().foo(5))
  }

  @Test def testRegressionJSClassWithSyntheticStaticMethodsIssue10563(): Unit = {
    val obj1 = new JSClassWithSyntheticStaticMethodsIssue10563(Some(3))
    assertEquals(3, obj1.y)
    assertEquals(8, obj1.foo(5))

    val obj2 = new JSClassWithSyntheticStaticMethodsIssue10563(None)
    assertEquals(-1, obj2.y)
    assertEquals(4, obj2.foo(5))
  }

  @Test def testJSNativeDefaultCtorParamIssue11592(): Unit = {
    assertEquals("foo", new RangeErrorIssue11592("foo").message)
    assertEquals("", new RangeErrorIssue11592().message)
  }

  @Test def testNonJVMCharsInClosureParametersIssue12507(): Unit = {
    def foo(`[-3, 3]`: Int): Int => Int = { x =>
      `[-3, 3]`
    }

    assertEquals(5, foo(5)(4))
  }

  @Test def defaultAccessorBridgesIssue12572(): Unit = {
    new MyPromiseIssue12572[Int](5)
  }

  @Test def desugarIdentCrashIssue13221(): Unit = {
    assertEquals(1, X_Issue13221.I.i)
    assertEquals(1, X_Issue13221.blah)
  }

  @Test def primitivePlusStringThatIsATermRefIssue13518(): Unit = {
    def charPlusString(x: String): String = 'a' + x
    assertEquals("abc", charPlusString("bc"))

    def intPlusString(x: String): String = 5 + x
    assertEquals("5bc", intPlusString("bc"))
  }
}

object RegressionTestScala3 {
  class OuterClassIssue10177 { // can also be trait
    trait ParentTrait { // must be trait, can be private
      def concreteMethod(x: Int): Int = x + 1 // must have a concrete method
    }

    private class ChildClass extends ParentTrait // must be class *and* private

    def foo(x: Int): Int = new ChildClass().concreteMethod(x)
  }

  @js.native
  @JSGlobal("RangeError")
  class RangeErrorIssue11592(msg: String = js.native) extends js.Object {
    val message: String = js.native
  }

  class MyPromiseIssue12572[T](t: T) extends js.Promise[T]((resolve, reject) => resolve(t)) {
    override def `then`[S](
        onFulfilled: js.Function1[T, S | js.Thenable[S]],
        onRejected: js.UndefOr[js.Function1[scala.Any, S | js.Thenable[S]]] = js.undefined): js.Promise[S] = {
      ???
    }

    override def `then`[S >: T](
        onFulfilled: Unit,
        onRejected: js.UndefOr[js.Function1[scala.Any, S | js.Thenable[S]]]): js.Promise[S] = {
      ???
    }

    override def `catch`[S >: T](
        onRejected: js.UndefOr[js.Function1[scala.Any, S | js.Thenable[S]]] = js.undefined): js.Promise[S] = {
      ???
    }
  }

  object X_Issue13221 extends Y_Issue13221 {
    object I {
      def i = 1
    }
  }

  abstract class Y_Issue13221 { self: X_Issue13221.type =>
    import I._
    def blah = i
  }
}

// This class needs to be at the top-level, not in an object, to reproduce the issue
class JSClassWithSyntheticStaticMethodsIssue10563(x: Option[Int]) extends js.Object {
  val y: Int = x.getOrElse(-1) // lambda in constructor

  def foo(z: Int): Int = x.getOrElse(-1) + z // lambda in method
}
