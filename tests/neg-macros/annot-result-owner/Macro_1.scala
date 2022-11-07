import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class insertVal extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    // Use of wrong owner
    val valSym = Symbol.newUniqueVal(tree.symbol, "definitionWithWrongOwner", TypeRepr.of[Unit], Flags.Private, Symbol.noSymbol)
    val valDef = ValDef(valSym, Some('{}.asTerm))
    List(valDef, tree)
