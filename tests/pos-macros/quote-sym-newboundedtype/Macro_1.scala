//> using options -experimental -Yno-experimental
import scala.quoted.*

inline def testMacro = ${ testImpl }

transparent inline def transparentTestMacro = ${ testImpl }

def testImpl(using Quotes): Expr[Object] = {
    import quotes.reflect.*

    def makeType(owner: Symbol): Symbol =
      Symbol.newBoundedType(owner, "mytype", Flags.EmptyFlags, TypeBounds.lower(TypeRepr.of[String]), Symbol.noSymbol)

    val typeDef = TypeDef(makeType(Symbol.spliceOwner))
    // Expr printer does not work here, see comment:
    // https://github.com/scala/scala3/pull/20347#issuecomment-2096824617
    assert(typeDef.toString == "TypeDef(mytype,TypeTree[TypeBounds(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class java)),object lang),String),TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any))])")

    val clsSymbol = Symbol.newClass(Symbol.spliceOwner, "CLS", List(TypeRepr.of[Object]), sym => List(makeType(sym)), None)
    val classDef: ClassDef = ClassDef(clsSymbol, List(TypeTree.of[Object]), List(TypeDef(clsSymbol.typeMember("mytype"))))
    Block(List(classDef), Apply(Select(New(TypeIdent(clsSymbol)), clsSymbol.primaryConstructor), List.empty)).asExprOf[Object]
}
