package org.scalajs.testsuite.jsinterop

import org.junit.Assert.*
import org.junit.Test

import scala.scalajs.js
import scala.scalajs.js.annotation.*

object ExportedJSNativeMembersScala3:

  object A {

    @js.native
    trait FooModule extends js.Any { self: Foo.type =>
      val foo: String
    }

    @js.native
    @JSGlobal("Foo_GlobalThatWillBeExported")
    val Foo: FooModule = js.native

    @js.native
    @JSGlobal("Bar_GlobalThatWillBeExported")
    object Bar extends js.Any {
      val bar: Int = js.native
    }

    @js.native
    @JSGlobal("Baz_GlobalThatWillBeExported")
    final class Baz(var baz: String) extends js.Object

    @js.native
    @JSGlobal("QuxHolder_GlobalThatWillBeExported")
    final class QuxHolder(val qux: String) extends js.Object

    @js.native
    @JSGlobal("QuxHolderHolder_GlobalThatWillBeExported")
    final class QuxHolderHolder(val quxHolder: QuxHolder) extends js.Object {
      val qux: quxHolder.qux.type = js.native
    }

    @js.native // structurally equivalent to QuxHolderHolder, but a trait
    trait QuxHolderHolderTrait(val quxHolder: QuxHolder) extends js.Any {
      val qux: quxHolder.qux.type
    }

    @js.native
    @JSGlobal("quxxInstance_GlobalThatWillBeExported")
    val quxxInstance: QuxHolderHolderTrait = js.native

    @js.native
    @JSGlobal("addOne_GlobalThatWillBeExported")
    def addOne(i: Int): Int = js.native

  }

  object B extends js.Object {
    export A.FooModule            // trait  (native)
    export A.Foo                  // val    (native)
    export A.Bar                  // object (native)
    export A.Baz                  // class  (native)
    export A.QuxHolder            // class  (native)
    export A.QuxHolderHolder      // class  (native)
    export A.QuxHolderHolderTrait // trait  (native)
    export A.quxxInstance         // val    (native)
    export A.addOne               // def    (native)
  }

  final class C extends js.Object {
    export A.FooModule            // trait  (native)
    export A.Foo                  // val    (native)
    export A.Bar                  // object (native)
    export A.Baz                  // class  (native)
    export A.QuxHolder            // class  (native)
    export A.QuxHolderHolder      // class  (native)
    export A.QuxHolderHolderTrait // trait  (native)
    export A.quxxInstance         // val    (native)
    export A.addOne               // def    (native)
  }

class ExportedJSNativeMembersScala3:
  import ExportedJSNativeMembersScala3.*

  @Test def forward_top_level_JS_var_with_export(): Unit = {
    js.eval("""
      var Foo_GlobalThatWillBeExported = {
        foo: "foo"
      }
      var Bar_GlobalThatWillBeExported = {
        bar: 23
      }
      function Baz_GlobalThatWillBeExported(baz) {
        this.baz = baz
      }
      function QuxHolder_GlobalThatWillBeExported(qux) {
        this.qux = qux
      }
      function QuxHolderHolder_GlobalThatWillBeExported(quxHolder) {
        this.quxHolder = quxHolder;
        this.qux = quxHolder.qux;
      }
      var quxxInstance_GlobalThatWillBeExported = (
        new QuxHolderHolder_GlobalThatWillBeExported(
          new QuxHolder_GlobalThatWillBeExported("quxxInstance")
        )
      )
      function addOne_GlobalThatWillBeExported(i) {
        return i + 1;
      }
    """)

    val C = ExportedJSNativeMembersScala3.C()

    assertEquals("foo", A.Foo.foo)
    assertEquals("foo", B.Foo.foo)
    assertEquals("foo", C.Foo.foo)

    assertEquals(23, A.Bar.bar)
    assertEquals(23, B.Bar.bar)
    assertEquals(23, C.Bar.bar)

    val abaz = A.Baz("abaz1")
    assertEquals("abaz1", abaz.baz)
    abaz.baz = "abaz2"
    assertEquals("abaz2", abaz.baz)

    val bbaz = B.Baz("bbaz1")
    assertEquals("bbaz1", bbaz.baz)
    bbaz.baz = "bbaz2"
    assertEquals("bbaz2", bbaz.baz)

    val cbaz = C.Baz("cbaz1")
    assertEquals("cbaz1", cbaz.baz)
    cbaz.baz = "cbaz2"
    assertEquals("cbaz2", cbaz.baz)

    val quxHolderHolderA = A.QuxHolderHolder(A.QuxHolder("quxHolderHolderA"))
    assertEquals("quxHolderHolderA", quxHolderHolderA.qux)
    assertEquals("quxHolderHolderA", quxHolderHolderA.quxHolder.qux)

    val quxHolderHolderB = B.QuxHolderHolder(B.QuxHolder("quxHolderHolderB"))
    assertEquals("quxHolderHolderB", quxHolderHolderB.qux)
    assertEquals("quxHolderHolderB", quxHolderHolderB.quxHolder.qux)

    val quxHolderHolderC = C.QuxHolderHolder(C.QuxHolder("quxHolderHolderC"))
    assertEquals("quxHolderHolderC", quxHolderHolderC.qux)
    assertEquals("quxHolderHolderC", quxHolderHolderC.quxHolder.qux)

    assertEquals("quxxInstance", A.quxxInstance.qux)
    assertEquals("quxxInstance", A.quxxInstance.quxHolder.qux)
    assertEquals("quxxInstance", B.quxxInstance.qux)
    assertEquals("quxxInstance", B.quxxInstance.quxHolder.qux)
    assertEquals("quxxInstance", C.quxxInstance.qux)
    assertEquals("quxxInstance", C.quxxInstance.quxHolder.qux)

    assertEquals(2, A.addOne(1))
    assertEquals(3, B.addOne(2))
    assertEquals(4, C.addOne(3))
  }

end ExportedJSNativeMembersScala3
