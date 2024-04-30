//> using options -experimental -Yno-experimental

import scala.annotation.MacroAnnotation
import scala.quoted.*

class companionToString(str: String) extends MacroAnnotation:

  def transform(using Quotes)(
    definition: quotes.reflect.Definition,
    companion: Option[quotes.reflect.Definition]
  ): List[quotes.reflect.Definition] =

    import quotes.reflect.*
    companion match
      case Some(cls@ClassDef(name, ctr, parents, self, body)) =>
        val symbol = cls.symbol
        val toStringSym = Symbol.requiredMethod("java.lang.Object.toString")
        val toStringOverrideSym = Symbol.newMethod(symbol, "toString", toStringSym.info, Flags.Override, Symbol.noSymbol)
        val toStringDef = DefDef(toStringOverrideSym, _ => Some(Literal(StringConstant(s"$name: $str"))))
        val newCompanion = ClassDef.copy(cls)(name, ctr, parents, self, toStringDef :: body)
        List(definition, newCompanion)
      case Some(unexpected) =>
        report.error(s"Unexpected companion: ${unexpected.show}")
        List(definition)
      case None =>
        report.error("Companion is not available to transform")
        List(definition)
  end transform