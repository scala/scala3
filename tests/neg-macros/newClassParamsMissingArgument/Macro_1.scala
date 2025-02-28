//> using options -experimental

import scala.quoted._

inline def makeClass(inline name: String): Object = ${ makeClassExpr('name) }
private def makeClassExpr(nameExpr: Expr[String])(using Quotes): Expr[Object] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort
  val parents = List(TypeTree.of[Object])
  def decls(cls: Symbol): List[Symbol] = Nil

  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = _ => parents.map(_.tpe), decls, selfType = None, Flags.EmptyFlags, Symbol.noSymbol, List(("idx", TypeRepr.of[Int])))

  val clsDef = ClassDef(cls, parents, body = Nil)
  val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Object])

  Block(List(clsDef), newCls).asExprOf[Object]

  // '{
  //   class `name`(idx: Int)
  //   new `name`
  // }
}
