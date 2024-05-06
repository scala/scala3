import scala.quoted.*
    
inline def testMacro = ${ testImpl }

def testImpl(using Quotes): Expr[Unit] = { 
    import quotes.reflect.*
    val sym = Symbol.newType(Symbol.spliceOwner, "mytype", Flags.EmptyFlags, TypeRepr.of[String], Symbol.noSymbol)
    assert(TypeDef(sym).show == "type mytype = java.lang.String")
    '{ () }
}