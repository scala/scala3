import scala.quoted.*

inline def makeClass(inline name: String): Any = ${ makeClassExpr('name) }
private def makeClassExpr(nameExpr: Expr[String])(using Quotes): Expr[Any] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort
  val parents = List.empty[Tree] // BUG: first parent is not a class
  def decls(cls: Symbol): List[Symbol] = Nil

  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = Nil, decls, selfType = None)
  val clsDef = ClassDef(cls, parents, body = List())
  val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Object])

  Block(List(clsDef), newCls).asExpr

  // '{
  //   class `name`() {
  //     def foo(): Unit = println("Calling `name`.foo")
  //   }
  //   new `name`()
  // }
}
