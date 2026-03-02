//> using options -experimental -Yno-experimental
import scala.quoted.*

inline def testMacro = ${ testImpl }

transparent inline def transparentTestMacro = ${ testImpl }

def testImpl(using Quotes): Expr[Object] = {
  import quotes.reflect.*

  def makeBasicType(owner: Symbol): Symbol =
    Symbol.newTypeAlias(owner, "tpe", Flags.EmptyFlags, TypeRepr.of[String], Symbol.noSymbol)

  def makeTypesForClass(owner: Symbol): List[Symbol] =
    val typeLambda = TypeLambda.apply(List("X"), _ => List(TypeBounds.empty), _ => TypeRepr.of[Int])
    List(
      makeBasicType(owner),
      // type Foo = [X] =>> Int
      Symbol.newTypeAlias(
        owner,
        "tpe1",
        Flags.EmptyFlags,
        typeLambda,
        Symbol.noSymbol
      ),
    )

  val clsSymbol = Symbol.newClass(Symbol.spliceOwner, "CLS", List(TypeRepr.of[Object]), sym => makeTypesForClass(sym), None)
  val classDef: ClassDef = ClassDef(clsSymbol, List(TypeTree.of[Object]), List(
    TypeDef(clsSymbol.typeMember("tpe")),
    TypeDef(clsSymbol.typeMember("tpe1")),
  ))

  Block(List(classDef), Apply(Select(New(TypeIdent(clsSymbol)), clsSymbol.primaryConstructor), List.empty)).asExprOf[Object]
}
