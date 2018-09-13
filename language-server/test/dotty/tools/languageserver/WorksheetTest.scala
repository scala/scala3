package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.languageserver.util.Code._

class WorksheetTest {

  @Test def evaluateExpression: Unit = {
    ws"${m1}2 + 2".withSource
      .evaluate(m1, "1:val res0: Int = 4")
  }

  @Test def evaluateSimpleVal: Unit = {
    ws"${m1}val foo = 123".withSource
      .evaluate(m1, "1:val foo: Int = 123")
  }

  @Test def usePreviousDefinition: Unit = {
    ws"""${m1}val foo = 123
         val bar = foo + 1""".withSource
      .evaluate(m1, "1:val foo: Int = 123",
                    "2:val bar: Int = 124")
  }

  @Test def defineObject: Unit = {
    ws"""${m1}def foo(x: Int) = x + 1
         foo(1)""".withSource
      .evaluate(m1, "1:def foo(x: Int): Int",
                    "2:val res0: Int = 2")
  }

  @Test def defineCaseClass: Unit = {
    ws"""${m1} case class Foo(x: Int)
         Foo(1)""".withSource
      .evaluate(m1, "1:// defined case class Foo",
                    "2:val res0: Foo = Foo(1)")
  }

  @Test def defineClass: Unit = {
    ws"""${m1}class Foo(x: Int) {
           override def toString: String = "Foo"
         }
         new Foo(1)""".withSource
      .evaluate(m1, "3:// defined class Foo",
                    "4:val res0: Foo = Foo")
  }

  @Test def defineAnonymousClass0: Unit = {
    ws"""${m1}new {
          override def toString: String = "Foo"
         }""".withSource
      .evaluate(m1, "3:val res0: Object = Foo")
  }

  @Test def defineAnonymousClass1: Unit = {
    ws"""${m1}class Foo
         trait Bar
         new Foo with Bar {
           override def toString: String = "Foo"
         }""".withSource
      .evaluate(m1, "1:// defined class Foo",
                    "2:// defined trait Bar",
                    "5:val res0: Foo & Bar = Foo")
  }

  @Test def produceMultilineOutput: Unit = {
    ws"""${m1}1 to 3 foreach println""".withSource
      .evaluate(m1, "1:1\n2\n3")
  }

  @Test def patternMatching0: Unit = {
    ws"""${m1}1 + 2 match {
          case x if x % 2 == 0 => "even"
          case _ => "odd"
        }""".withSource
      .evaluate(m1, "4:val res0: String = odd")
  }

  @Test def patternMatching1: Unit = {
    ws"""${m1}val (foo, bar) = (1, 2)""".withSource
      .evaluate(m1, "1:val foo: Int = 1\nval bar: Int = 2")
  }

}
