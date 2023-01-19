import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class addTopLevelVal extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
      case ClassDef(name, ctr, parents, self, body) =>
        val valSym = Symbol.newVal(Symbol.spliceOwner, Symbol.freshName("toLevelVal"), TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
        val valDef = ValDef(valSym, Some(Literal(IntConstant(1))))
        List(valDef, tree)
      case _ =>
        report.error("Annotation only supports `class`")
        List(tree)
