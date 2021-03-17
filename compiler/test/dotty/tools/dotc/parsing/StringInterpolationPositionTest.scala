package dotty.tools
package dotc
package parsing

import ast.untpd._
import org.junit.Test

class StringInterpolationPositionTest extends ParserTest {

  val tq = "\"\"\""
  val program = s"""
    |class A {
    |  val expr = 42
    |  val s0 = s"string1"
    |  val s1 = s"string1$${expr}string2"
    |  val s2 = s"string1$${expr}string2$${expr}string3"
    |  val s0m = s${tq}string1${tq}
    |  val s1m = s${tq}string1$${expr}string2${tq}
    |  val s2m = s${tq}string1$${expr}string2$${expr}string3${tq}
    |}""".stripMargin

  @Test
  def interpolationLiteralPosition: Unit = {
    val t = parseText(program)
    t match {
      case PackageDef(_, List(TypeDef(_, tpl: Template))) => {
        val interpolations = tpl.body.collect{ case ValDef(_, _, InterpolatedString(_, int)) => int }
        val lits = interpolations.flatten.flatMap {
          case l @ Literal(_) => List(l)
          case Thicket(trees) => trees.collect { case l @ Literal(_) => l }
        }
        for {
          lit <- lits
          Literal(c) = lit
          str <- List(c.value).collect { case str: String => str}
        } {
          val fromPos = program.substring(lit.span.start, lit.span.end)
          assert(fromPos == str, s"$fromPos == $str")
        }
      }
    }
  }
}