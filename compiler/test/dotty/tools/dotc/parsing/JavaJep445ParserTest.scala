package dotty.tools.dotc.parsing

import dotty.tools.DottyTest
import dotty.tools.dotc.ast.Trees.{Ident, PackageDef, TypeDef}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.ModuleDef
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.StdNames.tpnme
import dotty.tools.dotc.printing.{PlainPrinter, Printer}
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.PlainFile
import org.junit.Assert.{assertTrue, fail}
import org.junit.Test
import JavaParsers.JavaParser

class JavaJep445ParserTest extends DottyTest {

  @Test def `the parser produces same trees for a class and an equivalent unnamed class`
      : Unit = {

    val unnamed = JavaParser(
      SourceFile.virtual(
        "MyUnnamed.java",
        s"""
          |import some.pkg.*;
          |
          |@interface InnerAnnotation {}
          |
          |interface InnerInterface {}
          |
          |@Magic
          |public volatile double d;
          |
          |void main() {}
          |
          |interface SecondInnerInterface {}
          |
          |""".stripMargin
      )
    ).parse()

    val named = JavaParser(
      SourceFile.virtual(
        "SomeFile.java",
        s"""
          |import some.pkg.*;
          |
          |private final class MyUnnamed {
          |
          |  @interface InnerAnnotation {}
          |  
          |  interface InnerInterface {}
          |  
          |  @Magic
          |  public volatile double d;
          |  
          |  void main() {}
          |  
          |  interface SecondInnerInterface {}
          |
          |}
          |""".stripMargin
      )
    ).parse()

    assertTrue(
      "expected same trees for named and unnamed classes",
      unnamed.sameTree(named)
    )
  }
}
