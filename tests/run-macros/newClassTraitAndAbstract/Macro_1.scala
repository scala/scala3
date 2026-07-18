import scala.quoted.*

transparent inline def makeTrait(inline name: String): Any = ${ makeTraitExpr('name) }
transparent inline def makeAbstractClass(inline name: String): Any = ${ makeAbstractClassExpr('name) }

private def makeTraitExpr(name: Expr[String])(using Quotes): Expr[Any] = {
  makeClassExpr(name, quotes.reflect.Flags.Trait, List(quotes.reflect.TypeTree.of[Object]))
  // '{
  //   trait `name`[A, B <: Int](param1: A, param2: B)
  //   class anon() extends `name`[String, Int]("a", 1)
  //   new $anon()
  // }
}

private def makeAbstractClassExpr(name: Expr[String])(using Quotes): Expr[Any] = {
  makeClassExpr(name, quotes.reflect.Flags.Abstract, Nil)
  // '{
  //   abstract class `name`[A, B <: Int](param1: A, param2: B)
  //   class anon() extends `name`[String, Int]("a", 1)
  //   new $anon()
  // }
}

private def makeClassExpr(using Quotes)(
    nameExpr: Expr[String], clsFlags: quotes.reflect.Flags, childExtraParents: List[quotes.reflect.TypeTree]
  ): Expr[Any] = {
  import quotes.reflect.*

  val name = nameExpr.valueOrAbort
  def decls(cls: Symbol): List[Symbol] = Nil
  val conMethodType =
    (classType: TypeRepr) => PolyType(List("A", "B"))(
      _ => List(TypeBounds.empty, TypeBounds.upper(TypeRepr.of[Int])),
      polyType => MethodType(List("param1", "param2"))((_: MethodType) => List(polyType.param(0), polyType.param(1)), (_: MethodType) =>
        AppliedType(classType, List(polyType.param(0), polyType.param(1)))
      )
    )

  val traitSymbol = Symbol.newClass(
    Symbol.spliceOwner,
    name,
    parents = _ => List(TypeRepr.of[Object]),
    decls,
    selfType = None,
    clsFlags,
    clsPrivateWithin = Symbol.noSymbol,
    clsAnnotations = Nil,
    conMethodType,
    conFlags = Flags.EmptyFlags,
    conPrivateWithin = Symbol.noSymbol,
    conParamFlags = List(List(Flags.EmptyFlags, Flags.EmptyFlags), List(Flags.EmptyFlags, Flags.EmptyFlags)),
    conParamPrivateWithins = List(List(Symbol.noSymbol, Symbol.noSymbol), List(Symbol.noSymbol, Symbol.noSymbol))
  )
  val traitDef = ClassDef(traitSymbol, List(TypeTree.of[Object]), body = Nil)

  val traitTypeTree = Applied(TypeIdent(traitSymbol), List(TypeTree.of[String], TypeTree.of[Int]))
  val clsSymbol = Symbol.newClass(
    Symbol.spliceOwner,
    "anon",
    parents = _ => childExtraParents.map(_.tpe) ++ List(traitTypeTree.tpe),
    decls = _ => Nil,
    selfType = None,
    clsFlags = Flags.EmptyFlags,
    clsPrivateWithin = Symbol.noSymbol,
    clsAnnotations = Nil,
    conMethodType = (classType: TypeRepr) => MethodType(Nil)(_ => Nil, _ => classType),
    conFlags = Flags.EmptyFlags,
    conPrivateWithin = Symbol.noSymbol,
    conParamFlags = List(List()),
    conParamPrivateWithins = List(List(Symbol.noSymbol, Symbol.noSymbol), List(Symbol.noSymbol, Symbol.noSymbol))
  )
  val obj = '{new java.lang.Object()}.asTerm match
    case Inlined(_, _, term) => term

  val parentsWithSym = childExtraParents ++ List(
    Apply(
      TypeApply(
        Select(New(traitTypeTree), traitSymbol.primaryConstructor),
        List(TypeTree.of[String], TypeTree.of[Int])
      ),
      List(Expr("a").asTerm, Expr(1).asTerm)
    )
  )
  val clsDef = ClassDef(clsSymbol, parentsWithSym, body = Nil)

  val newCls =
    Typed(
      Apply(
        Select(New(TypeIdent(clsSymbol)), clsSymbol.primaryConstructor),
        Nil
      ),
      TypeTree.of[Any]
    )
  val res = Block(List(traitDef), Block(List(clsDef), newCls)).asExpr

  Expr.ofTuple(res, Expr(res.show))
}
