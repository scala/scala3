import quoted.*
import scala.compiletime.testing.{typeChecks, typeCheckErrors}
import scala.compiletime.testing.{Error, ErrorKind}

transparent inline def assertCompiles(inline code: String): Unit =
  ${ assertCompilesImpl('code, '{typeCheckErrors(code)}) }

given FromExpr[ErrorKind] with {
  def unapply(expr: Expr[ErrorKind])(using Quotes) = expr match {
    case '{ ErrorKind.Parser } => Some(ErrorKind.Parser)
    case '{ ErrorKind.Typer }  => Some(ErrorKind.Typer)
    case _ => None
  }
}

given FromExpr[Error] with {
  def unapply(expr: Expr[Error])(using Quotes) = expr match {
    case '{ Error(${Expr(msg)}, ${Expr(line)}, ${Expr(col)}, ${Expr(kind)}) } => Some(Error(msg, line, col, kind))
    case _ => None
  }
}

private def assertCompilesImpl(self: Expr[_], typeChecked: Expr[List[Error]])(using Quotes): Expr[Unit] = {
  import quotes.reflect._

  def checkCompile(code: String): Expr[Unit] = {
    // For some reason `typeChecked.valueOrError` is failing here, so instead we grab
    // the varargs argument to List.apply and use that to extract the list of errors
    val errors = typeChecked.asTerm.underlyingArgument match {
      case Apply(TypeApply(Select(Ident("List"), "apply"), _), List(seq)) =>
        seq.asExprOf[Seq[Error]].valueOrError.toList
    }

    '{}
  }

  self.asTerm.underlyingArgument match {

    case Literal(StringConstant(code)) =>
      checkCompile(code.toString)

    case Apply(Select(_, "stripMargin"), List(Literal(StringConstant(code)))) =>
      checkCompile(code.toString.stripMargin)

    case _ =>
      report.throwError("The 'assertCompiles' function only works with String literals.")
  }
}