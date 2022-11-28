import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class addTopLevelMethod extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
      case ClassDef(name, ctr, parents, self, body) =>
        val methType = MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Int])
        val methSym = Symbol.newMethod(Symbol.spliceOwner, Symbol.freshName("toLevelMethod"), methType, Flags.EmptyFlags, Symbol.noSymbol)
        val methDef = DefDef(methSym, _ => Some(Literal(IntConstant(1))))
        List(methDef, tree)
      case _ =>
        report.error("Annotation only supports `class`")
        List(tree)
