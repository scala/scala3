package org.scalajs.testsuite.compiler

import org.junit.Assert.*
import org.junit.Test

import scala.concurrent.ExecutionContext.Implicits.{global => globalEc}
import scala.concurrent.Future

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.junit.async._

import org.scalajs.testsuite.jsinterop.ExportLoopback
import org.scalajs.testsuite.utils.Platform._

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

  @Test def defaultParamsInModuleDefWithBridgesIssue13860(): Unit = {
    import Issue13860._

    assertEquals(0L, Foo.bar().x)
    assertEquals(5L, Foo.bar(5L).x)
  }

  @Test def etaReduceIssue15494(): Unit = {
    // Also known as tests/run/i14623.scala for Scala/JVM

    object Thunk {
      private[this] val impl =
        ((x: Any) => x).asInstanceOf[(=> Any) => Function0[Any]]

      def asFunction0[A](thunk: => A): Function0[A] = impl(thunk).asInstanceOf[Function0[A]]
    }

    var i = 0
    val f1 = { () => i += 1; "" }
    assertSame(f1, Thunk.asFunction0(f1()))
    val f2 = { () => i += 1; i }
    assertSame(f2, Thunk.asFunction0(f2()))
    val f3 = { () => i += 1 }
    assertSame(f3, Thunk.asFunction0(f3()))
  }

  @Test def literalTypeJSNativeIssue16173(): Unit = {
    js.eval("""
      var RegressionTestScala3_Issue16173_foo = "constant";
      var RegressionTestScala3_Issue16173_bar = function() { return 5; };
    """)

    assertEquals("constant", Issue16173.foo1)
    assertEquals("constant", Issue16173.foo2)

    assertEquals(5, Issue16173.bar1())
  }

  @Test def mandatoryFieldsForSJSSemanticsInNonNativeJSClassIssue14168(): Unit = {
    val nonNativeJS = new Issue14168.NonNativeJSClass().asInstanceOf[js.Dynamic]
    assertEquals("string", nonNativeJS.stringField)
    assertEquals(null, nonNativeJS.nullField)
    assertEquals((), nonNativeJS.unitField)
    assertEquals(true, nonNativeJS.hasOwnProperty("unitField"))
  }

  @Test def mandatoryFieldsForSJSSemanticsInStaticExportsIssue14168(): Unit = {
    val staticExports = js.constructorOf[Issue14168.StaticExports]
    assertEquals("string", staticExports.stringField)
    assertEquals(null, staticExports.nullField)
    assertEquals((), staticExports.unitField)
    assertEquals(true, staticExports.hasOwnProperty("unitField"))
  }

  @Test def mandatoryFieldsForSJSSemanticsInTopLevelExportsIssue14168(): AsyncResult = await {
    if (isNoModule) {
      import js.Dynamic.global
      Future {
        assertEquals("string", global.RegressionTestScala3_Issue14168_stringField)
        assertEquals(null, global.RegressionTestScala3_Issue14168_nullField)
        assertEquals((), global.RegressionTestScala3_Issue14168_unitField)
      }
    } else {
      for (exports <- ExportLoopback.exportsNamespace) yield {
        assertEquals("string", exports.RegressionTestScala3_Issue14168_stringField)
        assertEquals(null, exports.RegressionTestScala3_Issue14168_nullField)
        assertEquals((), exports.RegressionTestScala3_Issue14168_unitField)
      }
    }
  }

  @Test def nonSelectJSNativeRHSIssue14289(): Unit = {
    js.eval("""
      var RegressionTestScala3_Issue14289 = {
        "a": function() { return "foo"; },
        "b": function() { return 5; },
        "c": function() { return true; }
      };
    """)

    assertEquals("foo", Issue14289.Container.a())
    assertEquals(5, Issue14289.Container.b())
    assertEquals(true, Issue14289.Container.c())
  }

  @Test def createArrayOfUnitIssue22226(): Unit = {
    val a = Array.ofDim[Unit](0)
    assertSame(classOf[Array[Unit]], a.getClass())

    val b = new Array[Unit](0)
    assertSame(classOf[Array[Unit]], b.getClass())

    val c = Array.ofDim[Unit](0, 0)
    assertSame(classOf[Array[Array[Unit]]], c.getClass())
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

  object Issue13860 {
    class Foo(var x: Long)

    trait Companion[A] {
      def bar(x: Long = 0): A
    }

    object Foo extends Companion[Foo] {
      def bar(x: Long = 0): Foo = new Foo(x)
    }
  }

  object Issue14896 {
    val obj = new js.Object {
      val a = 42
      val b = "foo"
    }

    val entries = js.Object.entries(obj)
    val js.Tuple2(k, v) = entries(0): @unchecked
  }

  object Issue16173 {
    @js.native
    @JSGlobal("RegressionTestScala3_Issue16173_foo")
    val foo1: "constant" = js.native

    @js.native
    @JSGlobal("RegressionTestScala3_Issue16173_foo")
    def foo2: "constant" = js.native

    @js.native
    @JSGlobal("RegressionTestScala3_Issue16173_bar")
    def bar1(): 5 = js.native
  }

  object Issue14168 {
    class NonNativeJSClass extends js.Object {
      val stringField: "string" = "string"
      val nullField: Null = null
      val unitField: Unit = ()
      final val finalValField = "finalVal"
    }

    class StaticExports extends js.Object

    object StaticExports {
      @JSExportStatic
      val stringField: "string" = "string"
      @JSExportStatic
      val nullField: Null = null
      @JSExportStatic
      val unitField: Unit = ()
      @JSExportStatic
      final val finalValField = "finalVal"
    }

    object TopLevelExports {
      @JSExportTopLevel("RegressionTestScala3_Issue14168_stringField")
      val stringField: "string" = "string"
      @JSExportTopLevel("RegressionTestScala3_Issue14168_nullField")
      val nullField: Null = null
      @JSExportTopLevel("RegressionTestScala3_Issue14168_unitField")
      val unitField: Unit = ()
      @JSExportTopLevel("RegressionTestScala3_Issue14168_finalValField")
      final val finalValField = "finalVal"
    }
  }

  object Issue14289 {
    import scala.scalajs.js.native
    import scala.scalajs.{js => renamedjs}
    import scala.scalajs.js.{native => renamednative}

    @js.native
    @js.annotation.JSGlobal("RegressionTestScala3_Issue14289")
    object Container extends js.Object {
      def a(): String = native
      def b(): Int = renamedjs.native
      def c(): Boolean = renamednative
    }
  }
}

// This class needs to be at the top-level, not in an object, to reproduce the issue
class JSClassWithSyntheticStaticMethodsIssue10563(x: Option[Int]) extends js.Object {
  val y: Int = x.getOrElse(-1) // lambda in constructor

  def foo(z: Int): Int = x.getOrElse(-1) + z // lambda in method
}
