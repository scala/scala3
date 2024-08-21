//> using options -experimental -Yno-experimental
import scala.quoted.*

inline def testMacro = ${ testImpl }

transparent inline def transparentTestMacro = ${ testImpl }

def testImpl(using Quotes): Expr[Object] = {
  import quotes.reflect.*

  def makeType(owner: Symbol): Symbol =
    Symbol.newTypeAlias(owner, "mytype", Flags.EmptyFlags, TypeRepr.of[String], Symbol.noSymbol)

  val clsSymbol = Symbol.newClass(Symbol.spliceOwner, "CLS", List(TypeRepr.of[Object]), sym => List(makeType(sym)), None)
  val classDef: ClassDef = ClassDef(clsSymbol, List(TypeTree.of[Object]), List(TypeDef(clsSymbol.typeMember("mytype"))))

  Block(List(classDef), Apply(Select(New(TypeIdent(clsSymbol)), clsSymbol.primaryConstructor), List.empty)).asExprOf[Object]
}
