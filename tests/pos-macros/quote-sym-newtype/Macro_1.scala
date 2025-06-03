//> using options -experimental -Yno-experimental
import scala.quoted.*

inline def testMacro = ${ testImpl }

def testImpl(using Quotes): Expr[Unit] = {
    import quotes.reflect.*
    val sym = Symbol.newTypeAlias(Symbol.spliceOwner, "mytype", Flags.EmptyFlags, TypeRepr.of[String], Symbol.noSymbol)
    val typeDef = TypeDef(sym)
    assert(typeDef.show == "type mytype = java.lang.String")

    Block(List(typeDef), '{()}.asTerm).asExprOf[Unit]
}
