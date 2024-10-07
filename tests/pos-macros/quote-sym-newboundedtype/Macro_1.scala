//> using options -experimental -Yno-experimental
import scala.quoted.*

inline def testMacro = ${ testImpl }

transparent inline def transparentTestMacro = ${ testImpl }

def testImpl(using Quotes): Expr[Object] = {
    import quotes.reflect.*

    def makeBasicType(owner: Symbol): Symbol =
      Symbol.newBoundedType(owner, "tpe", Flags.EmptyFlags, TypeBounds.lower(TypeRepr.of[String]), Symbol.noSymbol)

    def makeTypesForClass(owner: Symbol): List[Symbol] =
      val typeLambda = TypeLambda.apply(List("X"), _ => List(TypeBounds.empty), _ => TypeRepr.of[Int])
      List(
        makeBasicType(owner),
        // type Bla >: Nothing <: [X] =>> Int
        Symbol.newBoundedType(
          owner,
          "tpe1",
          Flags.EmptyFlags,
          TypeBounds.upper(typeLambda),
          Symbol.noSymbol
        ),
        // type Bar >: [X] =>> Int <: [X] =>> Int
        Symbol.newBoundedType(
          owner,
          "tpe2",
          Flags.EmptyFlags,
          TypeBounds(typeLambda, typeLambda),
          Symbol.noSymbol
        )
      )

    val typeDef = TypeDef(makeBasicType(Symbol.spliceOwner))
    // Expr printer does not work here, see comment:
    // https://github.com/scala/scala3/pull/20347#issuecomment-2096824617
    println(typeDef.toString)
    assert(typeDef.toString == "TypeDef(tpe,TypeTree[TypeBounds(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class java)),object lang),String),TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any))])")

    val clsSymbol = Symbol.newClass(Symbol.spliceOwner, "CLS", List(TypeRepr.of[Object]), sym => makeTypesForClass(sym), None)
    val classDef: ClassDef = ClassDef(clsSymbol, List(TypeTree.of[Object]), List(
      TypeDef(clsSymbol.typeMember("tpe")),
      TypeDef(clsSymbol.typeMember("tpe1")),
      TypeDef(clsSymbol.typeMember("tpe2")),
    ))
    Block(List(classDef), Apply(Select(New(TypeIdent(clsSymbol)), clsSymbol.primaryConstructor), List.empty)).asExprOf[Object]
}
