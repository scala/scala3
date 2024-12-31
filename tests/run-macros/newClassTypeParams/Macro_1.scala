//> using options -experimental

import scala.quoted.*

transparent inline def makeClass(inline name: String): Any = ${ makeClassExpr('name) }
private def makeClassExpr(nameExpr: Expr[String])(using Quotes): Expr[Any] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort
  def decls(cls: Symbol): List[Symbol] = Nil
  val constrType =
    (classType: TypeRepr) => PolyType(List("A", "B"))(
      _ => List(TypeBounds.empty, TypeBounds.upper(TypeRepr.of[Int])),
      polyType => MethodType(List("param1", "param2"))((_: MethodType) => List(polyType.param(0), polyType.param(1)), (_: MethodType) => classType)
    )

  val cls = Symbol.newClass(
    Symbol.spliceOwner,
    name,
    parents = _ => List(TypeRepr.of[Object]),
    decls,
    selfType = None,
    constrType,
    Flags.EmptyFlags,
    Symbol.noSymbol,
    Flags.EmptyFlags,
    Symbol.noSymbol,
    List(List(Flags.EmptyFlags, Flags.EmptyFlags), List(Flags.EmptyFlags, Flags.EmptyFlags))
  )

  val clsDef = ClassDef(cls, List(TypeTree.of[Object]), body = Nil)
  val newCls =
    cls.typeRef.asType match
      case '[t] =>
        Typed(Apply(TypeApply(Select(New(TypeIdent(cls)), cls.primaryConstructor), List(TypeTree.of[String], TypeTree.of[Int])), List(Expr("test").asTerm, Expr(1).asTerm)), TypeTree.of[Any])

  val res = Block(List(clsDef), newCls).asExpr

  Expr.ofTuple(res, Expr(res.show))

  // '{
  //   class `name`[A, B <: Int](param1: A, param2: B)
  //   new `name`[String, Int]("a", 1)
  // }
}
