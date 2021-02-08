package org.scalajs.testsuite.compiler

import org.junit.Assert.*
import org.junit.Test

class CustomReflectSelectableTestScala3 {
  import CustomReflectSelectableTestScala3.*

  @Test def selectField(): Unit = {
    val obj: reflect.Selectable { val x: Int } = new CustomReflectSelectable(42)
    assertEquals(47, obj.x)
  }

  @Test def callMethod(): Unit = {
    val obj: reflect.Selectable { def foo(x: Int, y: String): String } = new CustomReflectSelectable(42)
    assertEquals("3 bar 42", obj.foo(3, "bar"))
  }
}

object CustomReflectSelectableTestScala3 {
  class CustomReflectSelectable(param: Int) extends reflect.Selectable {
    val x: Int = 5 + param

    def foo(x: Int, y: String): String = s"$x $y $param"
  }
}
