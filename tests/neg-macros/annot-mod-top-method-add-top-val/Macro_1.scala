import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
// Assumes annotation is on top level def or val
class addTopLevelValOutsidePackageObject extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    val valSym = Symbol.newVal(Symbol.spliceOwner.owner, Symbol.freshName("toLevelVal"), TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
    val valDef = ValDef(valSym, Some(Literal(IntConstant(1))))
    List(valDef, tree)
