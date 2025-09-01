//> using options -experimental -Yno-experimental
import scala.quoted.*

inline def testConflictingBounds = ${ testConflictingBoundsImpl }
inline def testConflictingBoundsWithTypeLambda = ${ testConflictingBoundsWithTypeLambdaImpl }

transparent inline def transparentTestConflictingBounds = ${ testConflictingBoundsImpl }
transparent inline def transparentTestConflictingBoundsWithTypeLambda = ${ testConflictingBoundsWithTypeLambdaImpl }


def testConflictingBoundsImpl(using Quotes): Expr[Object] = {
  import quotes.reflect.*

  def makeType(owner: Symbol): Symbol =
    // type Foo >: Int <: String
    Symbol.newBoundedType(
      owner,
      "Foo",
      Flags.EmptyFlags,
      TypeBounds(TypeRepr.of[Int], TypeRepr.of[String]),
      Symbol.noSymbol
    )
  makeClass(makeType)
}

def testConflictingBoundsWithTypeLambdaImpl(using Quotes): Expr[Object] = {
  import quotes.reflect.*
  def makeType(owner: Symbol): Symbol =
    // type Foo >: [X] =>> Int <: Any
    Symbol.newBoundedType(
      owner,
      "Foo",
      Flags.EmptyFlags,
      TypeBounds(TypeLambda.apply(List("X"), _ => List(TypeBounds.empty), _ => TypeRepr.of[Int]), TypeRepr.of[Any]),
      Symbol.noSymbol
    )
  makeClass(makeType)
}

def makeClass(using quotes: Quotes)(typeCons: quotes.reflect.Symbol => quotes.reflect.Symbol) = {
  import quotes.reflect.*
  val clsSymbol = Symbol.newClass(Symbol.spliceOwner, "CLS", List(TypeRepr.of[Object]), sym => List(typeCons(sym)), None)
  val classDef: ClassDef = ClassDef(clsSymbol, List(TypeTree.of[Object]), List(TypeDef(clsSymbol.typeMember("Foo"))))

  Block(List(classDef), Apply(Select(New(TypeIdent(clsSymbol)), clsSymbol.primaryConstructor), List.empty)).asExprOf[Object]
}

