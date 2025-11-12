//> using options -experimental
import scala.quoted._
class Foo { type test1; val test2: List[Any] = List() }
object Macro:
  inline def inlineCall(): Any = ${impl}
  transparent inline def transparentInlineCall(): Any = ${impl}

  def impl(using Quotes): Expr[Any] =
    import quotes.reflect._
    val tpt = TypeTree.of[Foo]

    val typeDefSym = Symbol.newTypeAlias(Symbol.spliceOwner, "test1", Flags.EmptyFlags, TypeRepr.of[Int], Symbol.noSymbol)
    val valDefSym = Symbol.newVal(Symbol.spliceOwner, "test2", TypeRepr.of[List[Int]], Flags.EmptyFlags, Symbol.noSymbol)

    val defs = List(
      TypeDef(typeDefSym),
      ValDef(valDefSym, None)
    )
    val typeTree = Refined(tpt, defs, TypeRepr.of[Foo].typeSymbol)

    // A little redundant, but we only want to see if Refined.apply() works,
    // so we only replace the type with one we just constructed
    val body = '{
      new Foo { type test1 = Int; override val test2: List[Int] = List(0) }
    }
    val blockWithNewTypeTree = body.asTerm match
      case Inlined(_, _, Block(cDef : ClassDef, Typed(invocation, _))) =>
        Block(cDef, Typed(invocation, typeTree))

    blockWithNewTypeTree.asExpr
