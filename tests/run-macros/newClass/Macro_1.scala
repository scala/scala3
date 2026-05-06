import scala.quoted.*

inline def makeClass(inline name: String): Any = ${ makeClassExpr('name) }
private def makeClassExpr(nameExpr: Expr[String])(using Quotes): Expr[Any] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort
  val parents = List(TypeTree.of[Object])
  def decls(cls: Symbol): List[Symbol] = Nil

  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = parents.map(_.tpe), decls, selfType = None)

  val clsDef = ClassDef(cls, parents, body = List('{println(s"Constructing ${$nameExpr}")}.asTerm))
  val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Object])

  Block(List(clsDef), newCls).asExpr
  // '{
  //   class `name`() { println("Constructing `name`") }
  //   new `name`()
  // }
}
