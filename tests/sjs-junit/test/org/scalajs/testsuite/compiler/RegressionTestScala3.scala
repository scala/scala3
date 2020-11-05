package org.scalajs.testsuite.compiler

import org.junit.Assert._
import org.junit.Test

class RegressionTestScala3 {
  import RegressionTestScala3._

  @Test def testRegressionDoubleDefinitionOfOuterPointerIssue10177(): Unit = {
    assertEquals(6, new OuterClassIssue10177().foo(5))
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
}
