package org.scalajs.testsuite.jsinterop

import org.junit.Assert.*
import org.junit.Test

import org.scalajs.junit.async._

import scala.scalajs.js
import scala.scalajs.js.annotation.*

class SJSDynamicImportTestScala3 {
  import scala.concurrent.ExecutionContext.Implicits.global

  @Test def implicitThisReferenceInDynamicImport_Issue17344(): AsyncResult = await {
    class Foo() {
      def foo(): Int = 1
    }
    class Bar(foo: Foo) {
      def bar(): js.Promise[Int] = js.dynamicImport(foo.foo())
    }

    val bar = new Bar(new Foo())
    val promise = bar.bar()

    promise.toFuture.map { r =>
      assertEquals(1, r)
    }
  }
}
