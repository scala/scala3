//> using options -experimental

import scala.quoted.*

transparent inline def makeClass(inline name: String): JavaClass[Int] = ${ makeClassExpr('name) }
private def makeClassExpr(nameExpr: Expr[String])(using Quotes): Expr[JavaClass[Int]] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort
  val parents = List(TypeTree.of[JavaClass[Int]])
  def decls(cls: Symbol): List[Symbol] = Nil

  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = _ => parents.map(_.tpe), decls, selfType = None, Flags.EmptyFlags, Symbol.noSymbol, List(("idx", TypeRepr.of[Int])))

  val parentsWithSym = List(Apply(TypeApply(Select(New(TypeTree.of[JavaClass[Int]]), TypeRepr.of[JavaClass].typeSymbol.primaryConstructor), List(TypeTree.of[Int])), List(Ref(cls.fieldMember("idx")))))
  val clsDef = ClassDef(cls, parentsWithSym, body = Nil)
  val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), List(Literal(IntConstant(22)))), TypeTree.of[JavaClass[Int]])

  Block(List(clsDef), newCls).asExprOf[JavaClass[Int]]
  // '{
  //   class `name`(idx: Int) extends JavaClass[Int](idx)
  //   new `name`(22)
  // }
}