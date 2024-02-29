import scala.quoted.*

inline def myMacro = ${ myMacroImpl }
def myMacroImpl(using Quotes) =
  import quotes.reflect.*

  val valSym = Symbol.newVal(Symbol.spliceOwner, "foo", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
  val vdef = ValDef(valSym, None)
  vdef match
    case _: ValDef =>
      assert(vdef.tpt.tpe =:= TypeRepr.of[Int])
      assert(vdef.rhs == None)
  vdef match
    case vdef: ValOrDefDef =>
      assert(vdef.tpt.tpe =:= TypeRepr.of[Int])
      assert(vdef.rhs == None)

  val methSym = Symbol.newMethod(Symbol.spliceOwner, "bar", ByNameType(TypeRepr.of[Int]))
  val ddef = DefDef(methSym, _ => None)
  ddef match
    case _: DefDef =>
      assert(ddef.tpt.tpe =:= TypeRepr.of[Int])
      assert(ddef.rhs == None)
  ddef match
    case ddef: ValOrDefDef =>
      assert(ddef.tpt.tpe =:= TypeRepr.of[Int])
      assert(ddef.rhs == None)

  '{}
