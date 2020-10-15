package dotty.dokka.tasty

import scala.tasty.Reflection

trait SyntheticsSupport:
  self: TastyParser =>

  import reflect._

  extension (t: Type):
    def isTupleType: Boolean = hackIsTupleType(self.reflect)(t)

    def isCompiletimeAppliedType: Boolean = hackIsCompiletimeAppliedType(self.reflect)(t)

    def hackIsTupleType(r: Reflection)(rtpe: r.Type): Boolean = 
      import dotty.tools.dotc
      given ctx as dotc.core.Contexts.Context = r.rootContext.asInstanceOf
      val tpe = rtpe.asInstanceOf[dotc.core.Types.Type]
      ctx.definitions.isTupleType(tpe)

    def hackIsCompiletimeAppliedType(r: Reflection)(rtpe: r.Type): Boolean = 
      import dotty.tools.dotc
      given ctx as dotc.core.Contexts.Context = r.rootContext.asInstanceOf
      val tpe = rtpe.asInstanceOf[dotc.core.Types.Type]
      ctx.definitions.isCompiletimeAppliedType(tpe.typeSymbol)

  extension (s: Symbol):
    def isSyntheticFunc: Boolean = s.flags.is(Flags.Synthetic) || s.flags.is(Flags.FieldAccessor) || isDefaultHelperMethod

    def isSuperBridgeMethod: Boolean = s.name.contains("$super$")

    def isDefaultHelperMethod: Boolean = ".*\\$default\\$\\d+$".r.matches(s.name)

    def isOpaque: Boolean = hackIsOpaque(self.reflect)(s)

    def isInfix: Boolean = hackIsInfix(self.reflect)(s)
    
    def getAllMembers: List[Symbol] = hackGetAllMembers(self.reflect)(s)

  def isSyntheticField(c: Symbol, classDef: ClassDef) =
    c.flags.is(Flags.CaseAcessor) || c.flags.is(Flags.Object)

  def isSyntheticField(c: Symbol) = c.flags.is(Flags.CaseAcessor) || c.flags.is(Flags.Object)

  def isValidPos(pos: Position) =
    pos.exists && pos.start != pos.end

  def constructorWithoutParamLists(c: ClassDef): Boolean =
    !isValidPos(c.constructor.pos)  || {
      val end = c.constructor.pos.end
      val typesEnd =  c.constructor.typeParams.lastOption.fold(end - 1)(_.pos.end)
      val classDefTree = c.constructor.show
      c.constructor.typeParams.nonEmpty && end <= typesEnd + 1
    }

  // TODO: #49 Remove it after TASTY-Reflect release with published flag Extension
  def hackIsInfix(r: Reflection)(rsym: r.Symbol): Boolean = {
    import dotty.tools.dotc
    given ctx as dotc.core.Contexts.Context = r.rootContext.asInstanceOf
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    ctx.definitions.isInfix(sym)
  }
  /* We need there to filter out symbols with certain flagsets, because these symbols come from compiler and TASTY can't handle them well. 
  They are valdefs that describe case companion objects and cases from enum.
  TASTY crashed when calling _.tree on them.
  */
  def hackGetAllMembers(r: Reflection)(rsym: r.Symbol): List[r.Symbol] = {
    import dotty.tools.dotc
    given ctx as dotc.core.Contexts.Context = r.rootContext.asInstanceOf
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    sym.typeRef.appliedTo(sym.typeParams.map(_.typeRef)).allMembers.iterator.map(_.symbol)
      .collect {
         case sym if
          !sym.is(dotc.core.Flags.ModuleVal) &&
          !sym.flags.isAllOf(dotc.core.Flags.Enum | dotc.core.Flags.Case | dotc.core.Flags.JavaStatic) =>
              sym.asInstanceOf[r.Symbol]
      }.toList
  }

  def hackIsOpaque(r: Reflection)(rsym: r.Symbol): Boolean = {
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = r.rootContext.asInstanceOf
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    sym.is(dotc.core.Flags.Opaque)
  }

  def hackIsLeftAssoc(d: Symbol): Boolean = !d.name.endsWith(":")

  object MatchTypeCase:
    def unapply(tpe: Type): Option[(TypeOrBounds, TypeOrBounds)] =
      tpe match
        case AppliedType(t, Seq(from, to)) if t == MatchCaseType =>
            Some((from, to))
        case TypeLambda(paramNames, paramTypes, AppliedType(t, Seq(from, to))) if t == MatchCaseType =>
            Some((from, to))
        case _ =>
          None
