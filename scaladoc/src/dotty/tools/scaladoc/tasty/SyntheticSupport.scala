package dotty.tools.scaladoc
package tasty

import scala.quoted._

object SyntheticsSupport:

  extension (using Quotes)(t: reflect.TypeRepr)
    def isTupleType: Boolean = t.hackIsTupleType(t)

    def isCompiletimeAppliedType: Boolean = t.hackIsCompiletimeAppliedType(t)

    private def hackIsTupleType(rtpe: reflect.TypeRepr): Boolean =
      import dotty.tools.dotc
      given ctx: dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
      val tpe = rtpe.asInstanceOf[dotc.core.Types.Type]
      ctx.definitions.isTupleNType(tpe)

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

    def isInfix: Boolean = hackIsInfix(s)

    def getmembers: List[reflect.Symbol] = hackGetmembers(s)

  end extension

  def isValidPos(using Quotes)(pos: reflect.Position) =
    if hackExists(pos) then pos.start != pos.end else false

  def isSyntheticField(using Quotes)(c: reflect.Symbol) =
    import reflect._
    c.flags.is(Flags.CaseAccessor) || (c.flags.is(Flags.Module) && !c.flags.is(Flags.Given))

  def constructorWithoutParamLists(using Quotes)(c: reflect.ClassDef): Boolean =
    !isValidPos(c.constructor.pos)  || {
      val end = c.constructor.pos.end
      val typesEnd =  c.constructor.leadingTypeParams.lastOption.fold(end - 1)(_.pos.end)
      val classDefTree = c.constructor.show
      c.constructor.leadingTypeParams.nonEmpty && end <= typesEnd + 1
    }

  // TODO: #49 Remove it after TASTY-Reflect release with published flag Extension
  private def hackIsInfix(using Quotes)(rsym: reflect.Symbol): Boolean = {
    import reflect._
    import dotty.tools.dotc
    given ctx: dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    ctx.definitions.isInfix(sym)
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

  private def hackGetSupertypes(using Quotes)(rdef: reflect.ClassDef) = {
    import reflect._
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val classdef = rdef.asInstanceOf[dotc.ast.tpd.TypeDef]
    val ref = classdef.symbol.info.asInstanceOf[dotc.core.Types.ClassInfo].appliedRef
    val baseTypes: List[(dotc.core.Symbols.Symbol, dotc.core.Types.Type)] =
      ref.baseClasses.map(b => b -> ref.baseType(b))
    baseTypes.asInstanceOf[List[(Symbol, TypeRepr)]]
  }

  private def hackExists(using Quotes)(rpos: reflect.Position) = {
    import reflect._
    import dotty.tools.dotc
    import dotty.tools.dotc.util.Spans._
    given dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val pos = rpos.asInstanceOf[dotc.util.SourcePosition]
    pos.exists
  }

  def getSupertypes(using Quotes)(c: reflect.ClassDef) = hackGetSupertypes(c).tail

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
