//> using options -experimental

import scala.quoted._

inline def makeClass(inline name: String): Foo = ${ makeClassExpr('name) }
private def makeClassExpr(nameExpr: Expr[String])(using Quotes): Expr[Foo] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort
  val parents = List('{ new Foo(1) }.asTerm)
  def decls(cls: Symbol): List[Symbol] = Nil

  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = _ => parents.map(_.tpe), decls, selfType = None, List("idx"), List(TypeRepr.of[Int]), Flags.EmptyFlags, Symbol.noSymbol)

  val parentsWithSym = List(Apply(Select(New(TypeTree.of[Foo]), TypeRepr.of[Foo].typeSymbol.primaryConstructor), List(Ref(cls.fieldMember("idx")))))
  val clsDef = ClassDef(cls, parentsWithSym, body = Nil)
  val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), List(Literal(IntConstant(22)))), TypeTree.of[Foo])

  Block(List(clsDef), newCls).asExprOf[Foo]

  // '{
  //   class `name`(idx: Int) extends Foo(idx)
  //   new `name`(22)
  // }
}

class Foo(i: Int) {
  def foo(): Unit = println(s"Calling Foo.foo with i = $i")
}

