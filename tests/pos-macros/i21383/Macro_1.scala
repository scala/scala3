import scala.quoted.*

class MyAnnot(val name: String) extends scala.annotation.StaticAnnotation

@MyAnnot("hello")
class Foo

object Macro:
  transparent inline def annotOf[T]: String =
    ${ annotOfImpl[T] }

  def annotOfImpl[T: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    val annTerm = TypeRepr.of[T].typeSymbol.annotations
      .find(_.tpe =:= TypeRepr.of[MyAnnot]).get
    val annExpr = annTerm.asExprOf[MyAnnot]
    '{ $annExpr.name }
