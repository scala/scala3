import scala.quoted.*

inline def make[T](inline value: Int): T = ${impl[T]('value)}

def impl[T: Type](value: Expr[Int])(using Quotes): Expr[T] = {
  import quotes.reflect.*

  val className = "Foo"
  val parents = List(TypeTree.of[Object], TypeTree.of[T])

  val parentMethods = TypeRepr.of[T].typeSymbol.declaredMethods

  def decls(cls: Symbol): List[Symbol] =
    TypeRepr.of[T].typeSymbol.declaredFields.map(field => Symbol.newValOverride(cls, field)) ++
    TypeRepr.of[T].typeSymbol.declaredMethods.map(method => Symbol.newMethodOverride(cls, method))

  val cls = Symbol.newClass(Symbol.spliceOwner, className, parents = parents.map(_.tpe), decls, selfType = None)
  val body =
    cls.declaredFields.map { method => ValDef(method, Some(value.asTerm)) } ++
    cls.declaredMethods.map { method => DefDef(method, argss => Some(value.asTerm)) }
  val clsDef = ClassDef(cls, parents, body = body)
  val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[T])
  Block(List(clsDef), newCls).asExprOf[T]
}
