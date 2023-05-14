//> using scala "3.2.1"
import scala.quoted.Expr
import scala.quoted.Type
import scala.quoted.quotes
import scala.quoted.Quotes

object macros {

  inline transparent def mkNames[A]: List[Any] = ${ mkNamesImpl[A] }

  def mkNamesImpl[A: Type](using Quotes): Expr[List[Any]] = {
    import quotes.reflect._

    val fieldNames = TypeRepr.of[A].typeSymbol.declaredFields.map(_.name)

    val types = fieldNames
      .map { f =>
        val t1 = ConstantType(StringConstant(f))
        t1.asType match {
          case '[t1Type] => TypeRepr.of[(t1Type, "aa")]
        }
      }
      .reduceLeft[TypeRepr](OrType(_, _))

    types.asType match {
      case '[ttt] =>
        Expr.ofList[ttt](
          fieldNames.map { v =>
            Expr[(v.type, "aa")](v -> "aa").asExprOf[ttt] // error
          }
        )
    }
  }

}
