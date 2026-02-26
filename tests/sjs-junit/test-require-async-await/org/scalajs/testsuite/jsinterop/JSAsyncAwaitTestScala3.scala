package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.collection.mutable.ArrayBuffer

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.junit.async._

class JSAsyncAwaitTestScala3 {
  @Test
  def unitAsyncBlock_Issue25342(): AsyncResult = await {
    val buf = new ArrayBuffer[String]()

    val px = js.Promise.resolve[Int](5)

    buf += "before"
    val p = js.async {
      buf += "start async"
      val x = js.await(px)
      buf += s"got x: $x"
      ()
    }
    buf += "after"

    assertArrayEquals(
        Array[AnyRef]("before", "start async", "after"),
        buf.toArray[AnyRef])

    p.toFuture.map { _ =>
      assertArrayEquals(
          Array[AnyRef]("before", "start async", "after", "got x: 5"),
          buf.toArray[AnyRef])
    }
  }
}
