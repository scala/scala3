import scala.quoted.*

inline def wire[T]: T = ${ wireImpl[T] }
def wireImpl[T: Type](using q: Quotes): Expr[T] = {
  import q.reflect.*

  lazy val targetType = TypeRepr.of[T]
  val constructorValue = targetType.typeSymbol.primaryConstructor
  val constructionMethodTree: Term = {
    val ctor = Select(New(TypeIdent(targetType.typeSymbol)), constructorValue)
    if (targetType.typeArgs.isEmpty) ctor else ctor.appliedToTypes(targetType.typeArgs)
  }
  val constructorArgsValue = List(Nil)
  val code: Tree = constructorArgsValue.foldLeft(constructionMethodTree)((acc: Term, args: List[Term]) => Apply(acc, args))
  code.asExprOf[T]
}
