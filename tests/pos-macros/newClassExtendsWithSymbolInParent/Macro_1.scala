//> using options -experimental

import scala.quoted.*

transparent inline def makeClass(inline name: String): Foo[_] = ${ makeClassExpr('name) }
private def makeClassExpr(nameExpr: Expr[String])(using Quotes): Expr[Foo[_]] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort

  // using asType on the passed Symbol leads to cyclic reference errors
  def parents(cls: Symbol) =
    List(AppliedType(TypeRepr.typeConstructorOf(Class.forName("Foo")), List(TypeIdent(cls).tpe)))
  def decls(cls: Symbol): List[Symbol] = Nil

  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents, decls, selfType = None, Flags.EmptyFlags, Symbol.noSymbol, conParamNames = Nil, conParamTypes = Nil)

  val parentsWithSym =
    cls.typeRef.asType match
      case '[t] =>
        List(Apply(TypeApply(Select(New(TypeTree.of[Foo[t]]), TypeRepr.of[Foo[t]].typeSymbol.primaryConstructor), List(TypeTree.of[t])), List()))
  val clsDef = ClassDef(cls, parentsWithSym, body = Nil)

  val newCls = cls.typeRef.asType match
    case '[t] =>
      Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Foo[t]])

  cls.typeRef.asType match
    case '[t] =>
      Block(List(clsDef), newCls).asExprOf[Foo[t]]

  // '{
  //   class Name() extends Foo[Name.type]()
  //   new Name()
  // }
}

class Foo[X]() { self: X =>
  def getSelf: X = self
}
