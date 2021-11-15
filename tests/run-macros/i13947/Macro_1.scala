import scala.quoted.*

inline def printTypeParams[A]: Unit = ${ printTypeParamsImpl[A] }

def printTypeParamsImpl[A: Type](using q: Quotes): Expr[Unit] = {
  import q.reflect.*

  val targs: List[TypeRepr] = TypeRepr.of[A].typeArgs
  val debug = targs.map(_.show).mkString("[", ", ", "]")

  '{ println(${Expr(debug)}) }
}
