import scala.quoted.*

inline def printTypeParams[A]: Unit = ${ printTypeParamsImpl[A] }

def printTypeParamsImpl[A: Type](using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  val targs: List[TypeRepr] = TypeRepr.of[A].typeArgs
  val debug = targs.map(_.show).mkString("[", ", ", "]")

  '{ println(${Expr(debug)}) }
}
