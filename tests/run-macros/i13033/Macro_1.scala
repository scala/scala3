import scala.quoted.*

object Macro:
  def nanImpl(using Quotes): Expr[Any] =
    '{ Double.NaN } match
      case '{ Double.NaN } => '{0}
  inline def nan: Any = ${nanImpl}

  def positiveImpl(using Quotes): Expr[Any] =
    '{ 0.0 }  match
      case '{ 0.0 } => '{1}
  inline def positive: Any = ${positiveImpl}

  def negativeImpl(using Quotes): Expr[Any] =
    '{ -0.0 }  match
      case '{ -0.0 } => '{-1}
  inline def negative: Any = ${negativeImpl}
