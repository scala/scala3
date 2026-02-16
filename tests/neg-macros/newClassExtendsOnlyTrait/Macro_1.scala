import scala.quoted.*

inline def makeClass(inline name: String): Foo = ${ makeClassExpr('name) }
private def makeClassExpr(nameExpr: Expr[String])(using Quotes): Expr[Foo] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort
  val parents = List(TypeTree.of[Foo]) // BUG: first parent is not a class
  def decls(cls: Symbol): List[Symbol] =
    List(Symbol.newMethod(cls, "foo", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit])))

  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = parents.map(_.tpe), decls, selfType = None)
  val fooSym = cls.declaredMethod("foo").head

  val fooDef = DefDef(fooSym, argss => Some('{println(s"Calling ${$nameExpr}.foo")}.asTerm))
  val clsDef = ClassDef(cls, parents, body = List(fooDef))
  val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Foo])

  Block(List(clsDef), newCls).asExprOf[Foo]

  // '{
  //   class `name`() extends Foo {
  //     def foo(): Unit = println("Calling `name`.foo")
  //   }
  //   new `name`()
  // }
}

trait Foo {
  def foo(): Unit
}
