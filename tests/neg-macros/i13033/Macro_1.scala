import scala.quoted.*

object Macro:
  def positiveImpl(using Quotes): Expr[Any] =
    '{ 0.0 }  match
      case '{ -0.0 } => '{1}
  inline def positive: Any = ${positiveImpl}

  def negativeImpl(using Quotes): Expr[Any] =
    '{ -0.0 }  match
      case '{ 0.0 } => '{-1}
  inline def negative: Any = ${negativeImpl}
