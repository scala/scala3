package dotty.tools
package dotc
package parsing

import ast.untpd.*
import util.SourceFile
import org.junit.Test

class OutlineParserTest extends DottyTest {

  def parseOutline(code: String): Tree = {
    val source = SourceFile.virtual("<code>", code)
    val parser = new Parsers.OutlineParser(source)
    parser.parse()
  }

  @Test def `outline parser handles simple val`: Unit = {
    val code = "val x = 42"
    parseOutline(code)
  }

  @Test def `outline parser handles simple string interpolation`: Unit = {
    val code = """val x = s"hello $name""""
    parseOutline(code)
  }

  @Test def `outline parser handles string interpolation with block expression`: Unit = {
    val code = """val x = s"hello ${name.toString}""""
    parseOutline(code)
  }

  @Test def `outline parser handles raw string interpolation with complex expression`: Unit = {
    // This is the actual failing case from compiler/test/dotty/tools/utils.scala
    val code = """val toolArg = raw"(?://|/\*| \*) ?(?i:(${ToolName.values.mkString("|")})):((?:[^*]|\*(?!/))*)".r.unanchored"""
    parseOutline(code)
  }

  @Test def `outline parser handles multiline string interpolation`: Unit = {
    val tq = "\"\"\""
    val code = s"""val x = s${tq}hello $${name.toString}${tq}"""
    parseOutline(code)
  }

  @Test def `outline parser handles string interpolation with nested braces`: Unit = {
    val code = """val x = s"result: ${list.map { x => x + 1 }.mkString}""""
    parseOutline(code)
  }
}
