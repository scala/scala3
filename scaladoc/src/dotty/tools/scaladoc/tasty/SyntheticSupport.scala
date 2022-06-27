package dotty.tools.scaladoc
package tasty

import scala.quoted._

object SyntheticsSupport:

  extension (using Quotes)(s: quotes.reflect.Symbol)
    def isSyntheticFunc: Boolean =
      import quotes.reflect.*
      s.flags.is(Flags.Synthetic) || s.flags.is(Flags.FieldAccessor) || s.isDefaultHelperMethod

    def isSuperBridgeMethod: Boolean = s.name.contains("$super$")

    def isDefaultHelperMethod: Boolean = ".*\\$default\\$\\d+$".r.matches(s.name)

    def isOpaque: Boolean =
      import quotes.reflect.*
      s.flags.is(Flags.Opaque)

    def getmembers: List[quotes.reflect.Symbol] = hackGetmembers(s)

  end extension

  private def hackExists(using Quotes)(rpos: quotes.reflect.Position) = {
    import quotes.reflect.*
    import dotty.tools.dotc
    import dotty.tools.dotc.util.Spans._
    given dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val pos = rpos.asInstanceOf[dotc.util.SourcePosition]
    pos.exists
  }

  def isSyntheticField(using Quotes)(c: quotes.reflect.Symbol) =
    import quotes.reflect.*
    c.flags.is(Flags.CaseAccessor) || (c.flags.is(Flags.Module) && !c.flags.is(Flags.Given))

  def constructorWithoutParamLists(using Quotes)(c: quotes.reflect.ClassDef): Boolean =
    if hackExists(c.constructor.pos) then {
      c.constructor.pos.start == c.constructor.pos.end || {
        val end = c.constructor.pos.end
        val typesEnd =  c.constructor.leadingTypeParams.lastOption.fold(end - 1)(_.pos.end)
        val classDefTree = c.constructor.show
        c.constructor.leadingTypeParams.nonEmpty && end <= typesEnd + 1
      }
    } else false

  def getSupertypes(using Quotes)(c: quotes.reflect.ClassDef) =
    c.symbol.typeRef.baseClasses.map(b => b -> c.symbol.typeRef.baseType(b)).tail

  def typeForClass(using Quotes)(c: quotes.reflect.ClassDef): quotes.reflect.TypeRepr =
    c.symbol.typeRef.appliedTo(c.symbol.typeMembers.filter(_.isTypeParam).map(_.typeRef))

  /* We need there to filter out symbols with certain flagsets, because these symbols come from compiler and TASTY can't handle them well.
    They are valdefs that describe case companion objects and cases from enum.
    TASTY crashed when calling _.tree on them.
    */
  private def hackGetmembers(using Quotes)(rsym: quotes.reflect.Symbol): List[quotes.reflect.Symbol] = {
    import quotes.reflect.*
    import dotty.tools.dotc
    given ctx: dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    sym.namedType.allMembers.iterator.map(_.symbol)
      .collect {
         case sym if
          (!sym.is(dotc.core.Flags.ModuleVal) || sym.is(dotc.core.Flags.Given)) &&
          !sym.flags.isAllOf(dotc.core.Flags.Enum | dotc.core.Flags.Case | dotc.core.Flags.JavaStatic) =>
              sym.asInstanceOf[Symbol]
      }.toList
  }
