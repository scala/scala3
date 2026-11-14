import scala.deriving.Mirror
import scala.quoted.*

object Macros:
  /** Like type class derivation libraries do, reads the annotations of the parameters of the
   *  primary constructor of `T`, along with a mirror of `T`
   */
  inline def docsWithMirror[T](using Mirror.ProductOf[T]): String = ${ docsImpl[T] }

  /** Only reads the annotations of the parameters of the primary constructor of `T` */
  inline def docs[T]: String = ${ docsImpl[T] }

  def docsImpl[T: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    val texts =
      for
        param <- TypeRepr.of[T].typeSymbol.primaryConstructor.paramSymss.flatten
        annot <- param.annotations
        if annot.tpe <:< TypeRepr.of[doc]
      yield annot match
        case Apply(_, List(Literal(StringConstant(text)))) => text
        case _ => report.errorAndAbort(s"Unexpected annotation: ${annot.show}")
    Expr(texts.mkString(","))
