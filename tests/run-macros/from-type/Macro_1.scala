import scala.quoted.*

inline def testValueOfType[T]: Unit = ${ testValueOfTypeExpr[T] }

private def testValueOfTypeExpr[T: Type](using Quotes): Expr[Unit] =
  val value = Type.valueOfConstant[T]
  val strExpr = Expr(value.toString)
  '{ println($strExpr) }
