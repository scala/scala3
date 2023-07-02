import scala.annotation.StaticAnnotation

class SqlName(val sqlName: String) extends StaticAnnotation

import scala.compiletime.*
import scala.quoted.*

inline def sqlFieldNamesFor[T]: Vector[(String, String)] = ${
  sqlFieldNamesForImpl[T]
}

private def sqlFieldNamesForImpl[T: Type](using
    Quotes // must be named!! like `q: Quotes`
): Expr[Vector[(String, String)]] =
  import quotes.reflect.*
  val annot = TypeRepr.of[SqlName].typeSymbol
  val tuples: Seq[Expr[(String, String)]] = TypeRepr
    .of[T]
    .typeSymbol
    .primaryConstructor
    .paramSymss
    .head
    .collect:
      case sym if sym.hasAnnotation(annot) =>
        val fieldNameExpr = Expr(sym.name.asInstanceOf[String])
        val annotExpr = sym.getAnnotation(annot).get.asExprOf[SqlName]
        '{ ($fieldNameExpr, $annotExpr.sqlName) }
  val seq: Expr[Seq[(String, String)]] = Expr.ofSeq(tuples)
  '{ $seq.toVector }
