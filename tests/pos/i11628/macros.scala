// assume that this conversion utility is defined in Scala 2
class Scala2Conversion[T, V](val f: T => V)
object Scala2Conversion{
  implicit def create[T, V](implicit f: T => V): Scala2Conversion[T, V] = new Scala2Conversion(f)
}

// assume this utility in Scala 3, to summon a conversion within a macro
import quoted._
def summonConversionImpl(using qctx: Quotes): Expr[Any] = {
  import qctx.reflect._

  // hardcoded in this example to look for String to Int
  val conversionTpe = TypeRepr.of[Scala2Conversion[String, Int]]

  Implicits.search(conversionTpe) match {
    case iss: ImplicitSearchSuccess =>
      iss.tree.asExpr
    case isf: ImplicitSearchFailure =>
      report.error(s"can't find conversion")
      '{???}
  }
}

inline def summonConversion() = ${summonConversionImpl}
