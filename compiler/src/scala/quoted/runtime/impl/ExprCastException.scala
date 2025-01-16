package scala.quoted.runtime.impl


class ExprCastException(msg: String) extends Exception(msg)


object ExprCastException:
  def apply(expectedType: String, actualType: String, exprCode: String): ExprCastException =
    new ExprCastException(
      s"""|
          |  Expected type: ${formatLines(expectedType)}
          |  Actual type: ${formatLines(actualType)}
          |  Expression: ${formatLines(exprCode)}
          |""".stripMargin)

  private def formatLines(str: String): String =
    if !str.contains("\n") then str
    else str.linesIterator.mkString("\n    ", "\n    ", "\n")
