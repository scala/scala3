package dotty.tools.scaladoc
package tasty

import scala.quoted._

object SyntheticsSupport:

  extension (using Quotes)(t: reflect.TypeRepr)

    def isCompiletimeAppliedType: Boolean = t.hackIsCompiletimeAppliedType(t)

    private def hackIsCompiletimeAppliedType(rtpe: reflect.TypeRepr): Boolean =
      import dotty.tools.dotc
      given ctx: dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
      val tpe = rtpe.asInstanceOf[dotc.core.Types.Type]
      ctx.definitions.isCompiletimeAppliedType(tpe.typeSymbol)
  end extension

  extension (using Quotes)(s: reflect.Symbol)
    def isSyntheticFunc: Boolean =
      import reflect._
      s.flags.is(Flags.Synthetic) || s.flags.is(Flags.FieldAccessor) || s.isDefaultHelperMethod

    def isSuperBridgeMethod: Boolean = s.name.contains("$super$")

    def isDefaultHelperMethod: Boolean = ".*\\$default\\$\\d+$".r.matches(s.name)

    def isOpaque: Boolean =
      import reflect._
      s.flags.is(Flags.Opaque)

    def getmembers: List[reflect.Symbol] = hackGetmembers(s)

  end extension

  def isSyntheticField(using Quotes)(c: reflect.Symbol) =
    import reflect._
    c.flags.is(Flags.CaseAccessor) || (c.flags.is(Flags.Module) && !c.flags.is(Flags.Given))

  def constructorWithoutParamLists(using Quotes)(c: reflect.ClassDef): Boolean =
    c.constructor.pos.start == c.constructor.pos.end || {
      val end = c.constructor.pos.end
      val typesEnd =  c.constructor.leadingTypeParams.lastOption.fold(end - 1)(_.pos.end)
      val classDefTree = c.constructor.show
      c.constructor.leadingTypeParams.nonEmpty && end <= typesEnd + 1
    }

  /* We need there to filter out symbols with certain flagsets, because these symbols come from compiler and TASTY can't handle them well.
  They are valdefs that describe case companion objects and cases from enum.
  TASTY crashed when calling _.tree on them.
  */
  private def hackGetmembers(using Quotes)(rsym: reflect.Symbol): List[reflect.Symbol] = {
    import reflect._
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

  def getSupertypes(using Quotes)(c: reflect.ClassDef) =
    c.symbol.typeRef.baseClasses.map(b => b -> c.symbol.typeRef.baseType(b)).tail

  def typeForClass(using Quotes)(c: reflect.ClassDef): reflect.TypeRepr =
    import reflect._
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val cSym = c.symbol.asInstanceOf[dotc.core.Symbols.Symbol]
    cSym.typeRef.appliedTo(cSym.typeParams.map(_.typeRef)).asInstanceOf[TypeRepr]

  def memberInfo(using Quotes)(c: reflect.ClassDef, symbol: reflect.Symbol): reflect.TypeRepr =
    import reflect._
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    typeForClass(c).asInstanceOf[dotc.core.Types.Type]
      .memberInfo(symbol.asInstanceOf[dotc.core.Symbols.Symbol])
      .asInstanceOf[TypeRepr]
