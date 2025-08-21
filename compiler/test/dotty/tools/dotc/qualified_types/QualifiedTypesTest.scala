package dotty.tools.dotc.qualified_types

import dotty.tools.DottyTest
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.{Context, FreshContext}
import dotty.tools.dotc.core.Decorators.i

import org.junit.Assert.assertEquals
import org.junit.runners.MethodSorters
import org.junit.FixMethodOrder

@FixMethodOrder(MethodSorters.JVM)
abstract class QualifiedTypesTest extends DottyTest:

  override protected def initializeCtx(fc: FreshContext): Unit =
    super.initializeCtx(fc)
    fc.setSetting(fc.settings.XnoEnrichErrorMessages, true)
    fc.setSetting(fc.settings.color, "never")
    fc.setSetting(fc.settings.language, List("experimental.qualifiedTypes").asInstanceOf)

  def checkCompileExpr(statsString: String)(assertion: List[tpd.Tree] => Context ?=> Unit): Unit =
    checkCompile("typer", s"object Test { $statsString }"): (pkg, context) =>
      given Context = context
      val packageStats = pkg.asInstanceOf[tpd.PackageDef].stats
      val clazz = getTypeDef(packageStats, "Test$")
      val clazzStats = clazz.rhs.asInstanceOf[tpd.Template].body
      assertion(clazzStats)(using context)

  def getTypeDef(trees: List[tpd.Tree], name: String)(using Context): tpd.TypeDef =
    trees.collectFirst { case td: tpd.TypeDef if td.name.toString() == name => td }.get

  def getValDef(trees: List[tpd.Tree], name: String)(using Context): tpd.ValDef =
    trees.collectFirst { case vd: tpd.ValDef if vd.name.toString() == name => vd }.get

  def getDefDef(trees: List[tpd.Tree], name: String)(using Context): tpd.DefDef =
    trees.collectFirst { case vd: tpd.DefDef if vd.name.toString() == name => vd }.get

  def assertStringEquals(expected: String, found: String)(using Context): Unit =
    val formattedExpected = if expected.contains('\n') then "\n" + expected.linesIterator.map("    " + _).mkString("\n") else expected
    val formattedFound = if found.contains('\n') then "\n" + found.linesIterator.map("    " + _).mkString("\n") else found
    assertEquals(s"\n  Expected: $formattedExpected\n  Found:    $formattedFound\n", expected, found)
