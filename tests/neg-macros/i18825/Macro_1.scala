import scala.annotation.experimental
import scala.annotation.MacroAnnotation
import scala.quoted.*

@experimental
class toString extends MacroAnnotation :
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    tree match
      case ClassDef(name, ctr, parents, self, body) =>
        val cls = tree.symbol
        val toStringSym = Symbol.requiredMethod("java.lang.Object.toString")
        val toStringOverrideSym = Symbol.newMethod(cls, "toString", toStringSym.info, Flags.Override, Symbol.noSymbol)
        val toStringDef = DefDef(toStringOverrideSym, _ => Some(Literal(StringConstant("Hello from macro"))))
        val newClassDef = ClassDef.copy(tree)(name, ctr, parents, self, toStringDef :: body)
        List(newClassDef)
      case _ =>
        report.error("@toString can only be annotated on class definitions")
        tree :: Nil
