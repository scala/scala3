import scala.deriving.Mirror
import scala.compiletime.{constValue, error}
import scala.quoted.*

object TestMacro {
  inline def test1[CASE_CLASS <: Product](using m: Mirror.ProductOf[CASE_CLASS]): String =
    ${ code('m) }

  def code[CASE_CLASS <: Product: Type](m: Expr[Mirror.ProductOf[CASE_CLASS]])(using Quotes): Expr[String] =
    m match
      case '{ type t <: Tuple; $_ : Mirror { type MirroredElemLabels = `t` } } =>
        Expr(Type.valueOfTuple[t].toString)
}
