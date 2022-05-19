package dotty.tools
package dotc
package transform

import org.junit.Test

import dotty.tools.backend.jvm.DottyBytecodeTest

import scala.jdk.CollectionConverters.*

class SpecializeTuplesTests extends DottyBytecodeTest {
  @Test def noBoxing = {
    given source: String =
      """|class Test {
         |  def foo: (Int, Int) = (1, 1)
         |  def bar: Int = foo._1
         |}""".stripMargin

    checkBCode(source) { dir =>
      val methods = findClass("Test", dir).methods.nn
      assertNoBoxing("foo", methods)
      assertNoBoxing("bar", methods)
    }
  }

  @Test def boxing = {
    // Pick a tuple type that isn't specialised.
    given source: String =
      """|class Test {
         |  def t: (Boolean, Byte, Short, Char, Int, Long, Float, Double, AnyRef) = (true, 8, 16, 'c', 32, 64L, 32.0f, 64.0, this)
         |  def _1 = t._1
         |  def _2 = t._2
         |  def _3 = t._3
         |  def _4 = t._4
         |  def _5 = t._5
         |  def _6 = t._6
         |  def _7 = t._7
         |  def _8 = t._8
         |  def _9 = t._9
         |}""".stripMargin

    checkBCode(source) { dir =>
      val methods = findClass("Test", dir).methods.nn
      assertBoxing("t", methods)
      assertBoxing("_1", methods)
      assertBoxing("_2", methods)
      assertBoxing("_3", methods)
      assertBoxing("_4", methods)
      assertBoxing("_5", methods)
      assertBoxing("_6", methods)
      assertBoxing("_7", methods)
      assertBoxing("_8", methods)
      assertNoBoxing("_9", methods)
    }
  }
}
