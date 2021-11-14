import scala.quoted.*

inline def printTypeParams[A]: Unit = ${ printTypeParamsImpl[A] }

def printTypeParamsImpl[A: Type](using q: Quotes): Expr[Unit] = {
  import q.reflect.*

  val tparams: List[TypeRepr] = TypeRepr.of[A].typeParams
  val debug = tparams.map(_.show).mkString(", ")

  '{ println(${Expr(debug)}) }
}
