package dotty.tools.debug

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.SymUtils

import java.{util => ju}
import ju.function.Consumer

class ExpressionCompilerConfig private[debug] (
  packageName: String,
  outputClassName: String,
  private[debug] val breakpointLine: Int,
  private[debug] val expression: String,
  private[debug] val localVariables: ju.Set[String],
  private[debug] val errorReporter: Consumer[String],
  private[debug] val testMode: Boolean
):
  def this() = this(
    packageName = "",
    outputClassName = "",
    breakpointLine = -1,
    expression = "",
    localVariables = ju.Collections.emptySet,
    errorReporter = _ => (),
    testMode = false,
  )

  def withPackageName(packageName: String): ExpressionCompilerConfig = copy(packageName = packageName)
  def withOutputClassName(outputClassName: String): ExpressionCompilerConfig = copy(outputClassName = outputClassName)
  def withBreakpointLine(breakpointLine: Int): ExpressionCompilerConfig = copy(breakpointLine = breakpointLine)
  def withExpression(expression: String): ExpressionCompilerConfig = copy(expression = expression)
  def withLocalVariables(localVariables: ju.Set[String]): ExpressionCompilerConfig = copy(localVariables = localVariables)
  def withErrorReporter(errorReporter: Consumer[String]): ExpressionCompilerConfig = copy(errorReporter = errorReporter)

  private[debug] val expressionTermName: TermName = termName(outputClassName.toLowerCase)
  private[debug] val expressionClassName: TypeName = typeName(outputClassName)

  private[debug] def expressionClass(using Context): ClassSymbol =
    if packageName.isEmpty then requiredClass(outputClassName)
    else requiredClass(s"$packageName.$outputClassName")

  private[debug] def evaluateMethod(using Context): Symbol =
    expressionClass.info.decl(termName("evaluate")).symbol

  private def copy(
    packageName: String = packageName,
    outputClassName: String = outputClassName,
    breakpointLine: Int = breakpointLine,
    expression: String = expression,
    localVariables: ju.Set[String] = localVariables,
    errorReporter: Consumer[String] = errorReporter,
  ) = new ExpressionCompilerConfig(
    packageName,
    outputClassName,
    breakpointLine,
    expression,
    localVariables,
    errorReporter,
    testMode
  )
