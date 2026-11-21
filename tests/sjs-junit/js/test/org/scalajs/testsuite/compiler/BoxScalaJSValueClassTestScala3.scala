package org.scalajs.testsuite.compiler

import org.junit.Test

import scala.scalajs.js

class BoxScalaJSValueClassTestScala3:
  import BoxScalaJSValueClassMacros.*

  @Test def testBoxScalaJSValueClass(): Unit =
    val str = js.defined("ok")
    assert(str.nonEmpty)
