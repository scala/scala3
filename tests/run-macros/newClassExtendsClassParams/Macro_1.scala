import scala.quoted.*

inline def makeClass(inline name: String): Foo = ${ makeClassExpr('name) }
private def makeClassExpr(nameExpr: Expr[String])(using Quotes): Expr[Foo] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort
  val parents = List('{ new Foo(1) }.asTerm)
  def decls(cls: Symbol): List[Symbol] = Nil

  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = parents.map(_.tpe), decls, selfType = None)


  val clsDef = ClassDef(cls, parents, body = Nil)
  val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Foo])

  Block(List(clsDef), newCls).asExprOf[Foo]

  // '{
  //   class `name`() extends Foo(3)
  //   new `name`()
  // }
}

class Foo(i: Int) {
  def foo(): Unit = println(s"Calling Foo.foo with i = $i")
}
