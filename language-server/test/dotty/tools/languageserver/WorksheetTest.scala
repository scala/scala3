package dotty.tools.languageserver

import org.junit.Test
import org.eclipse.lsp4j.{CompletionItemKind, DocumentHighlightKind, SymbolKind}

import dotty.tools.languageserver.util.Code._
import dotty.tools.languageserver.util.embedded.CodeMarker

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

  @Test def evaluationException: Unit = {
    ws"""${m1}val foo = 1 / 0
         val bar = 2""".withSource
      .evaluateNonStrict(m1, "1:java.lang.ArithmeticException: / by zero",
                             "2:val bar: Int = 2")
  }

  @Test def worksheetCompletion(): Unit = {
    ws"""class Foo { def bar = 123 }
         val x = new Foo
         x.b${m1}""".withSource
      .completion(m1, Set(("bar", CompletionItemKind.Method, "=> Int")))
  }

  @Test def worksheetGoToDefinition(): Unit = {

    withSources(
      code"""class ${m11}Baz${m12}""",
      ws"""class ${m1}Foo${m2} { def ${m3}bar${m4} = new ${m5}Baz${m6}  }
           val x = new ${m7}Foo${m8}
           x.${m9}bar${m10}"""
     ).definition(m1 to m2, List(m1 to m2))
      .definition(m3 to m4, List(m3 to m4))
      .definition(m5 to m6, List(m11 to m12))
      .definition(m7 to m8, List(m1 to m2))
      .definition(m9 to m10, List(m3 to m4))
      .definition(m11 to m12, List(m11 to m12))
  }

  @Test def worksheetReferences(): Unit = {

    withSources(
      code"""class ${m11}Baz${m12}""",
      ws"""class ${m1}Foo${m2} { def ${m3}bar${m4} = new ${m9}Baz${m10}  }
           val x = new ${m5}Foo${m6}
           x.${m7}bar${m8}"""
    ).references(m1 to m2, List(m5 to m6))
     .references(m3 to m4, List(m7 to m8))
     .references(m11 to m12, List(m9 to m10))
  }

  @Test def worksheetRename(): Unit = {

    def sources =
      withSources(
        code"""class ${m9}Baz${m10}""",
        ws"""class ${m1}Foo${m2}(baz: ${m3}Baz${m4})
             val x = new ${m5}Foo${m6}(new ${m7}Baz${m8})"""
      )

    def testRenameFooFrom(m: CodeMarker) =
      sources.rename(m, "Bar", Set(m1 to m2, m5 to m6))

    def testRenameBazFrom(m: CodeMarker) =
      sources.rename(m, "Bar", Set(m3 to m4, m7 to m8, m9 to m10))

    testRenameFooFrom(m1)
    testRenameBazFrom(m3)
    testRenameFooFrom(m5)
    testRenameBazFrom(m7)
    testRenameBazFrom(m9)
  }

  @Test def worksheetHighlight(): Unit = {
    ws"""class ${m1}Foo${m2} { def ${m3}bar${m4} = 123  }
         val x = new ${m5}Foo${m6}
         x.${m7}bar${m8}""".withSource
      .highlight(m1 to m2, (m1 to m2, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read))
      .highlight(m3 to m4, (m3 to m4, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
      .highlight(m5 to m6, (m1 to m2, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read))
      .highlight(m7 to m8, (m3 to m4, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
  }

  def hoverContent(typeInfo: String, comment: String): Option[String] =
    Some(s"""```scala
            |$typeInfo
            |$comment
            |```""".stripMargin)
  @Test def worksheetHover(): Unit = {
    ws"""/** A class */ class ${m1}Foo${m2} { /** A method */ def ${m3}bar${m4} = 123  }
         val x = new ${m5}Foo${m6}
         x.${m7}bar${m8}""".withSource
      .hover(m1 to m2, hoverContent("Worksheet.Foo", "/** A class */"))
      .hover(m3 to m4, hoverContent("Int", "/** A method */"))
      .hover(m5 to m6, hoverContent("Worksheet.Foo", "/** A class */"))
      .hover(m7 to m8, hoverContent("Int", "/** A method */"))
  }

  @Test def worksheetDocumentSymbol(): Unit = {
    ws"""class ${m1}Foo${m2} {
           def ${m3}bar${m4} = 123
         }""".withSource
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Class, "Worksheet$"),
                          (m3 to m4).symInfo("bar", SymbolKind.Method, "Foo"))
  }

  @Test def worksheetSymbol(): Unit = {
    withSources(
      ws"""class ${m1}Foo${m2} {
             def ${m3}bar${m4} = 123
           }""",
      code"""class ${m5}Baz${m6}"""
    ).symbol("Foo", (m1 to m2).symInfo("Foo", SymbolKind.Class, "Worksheet$"))
     .symbol("bar", (m3 to m4).symInfo("bar", SymbolKind.Method, "Foo"))
     .symbol("Baz", (m5 to m6).symInfo("Baz", SymbolKind.Class))
  }

  @Test def worksheetCancel(): Unit = {
    ws"""${m1}val foo = 1
         val bar = 2
         while (true) {}
         val baz = 3""".withSource
      .cancelEvaluation(m1, afterMs = 5000)
  }

  @Test def systemExit(): Unit = {
    ws"""${m1}println("Hello, world!")
         System.exit(0)
         println("Goodbye!")""".withSource
      .evaluate(m1, "1:Hello, world!")
  }

  @Test def outputOnStdErr(): Unit = {
    ws"""${m1}System.err.println("Oh no")""".withSource
      .evaluate(m1, "1:Oh no")
  }

}
