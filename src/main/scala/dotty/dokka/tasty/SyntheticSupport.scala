package dotty.dokka.tasty

import scala.tasty.Reflection

trait SyntheticsSupport:
  self: TastyParser =>

  import reflect._

  def isValidPos(pos: Position) = 
    val a = (pos.exists, pos.start, pos.end)
    pos.exists && pos.start != pos.end

  def constructorWithoutParamLists(c: ClassDef): Boolean =
    !isValidPos(c.constructor.pos)  || {
      val end = c.constructor.pos.end
      val typesEnd =  c.constructor.typeParams.lastOption.fold(end - 1)(_.pos.end)
      val classDefTree = c.constructor.show
      c.constructor.typeParams.nonEmpty && end <= typesEnd + 1
    }

  def isSyntheticFunc(c: Symbol): Boolean =
    c.flags.is(Flags.Synthetic) || c.flags.is(Flags.FieldAccessor)  

  def isSyntheticField(c: Symbol, classDef: ClassDef) = 
    c.flags.is(Flags.CaseAcessor) || c.flags.is(Flags.Private) || c.flags.is(Flags.Object)

  def isExtensionMethod(d: Symbol): Boolean = hackIsExtension(self.reflect)(d)

  // TODO: #49 Remove it after TASTY-Reflect release with published flag Extension
  def hackIsExtension(r: Reflection)(rsym: r.Symbol): Boolean = {
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = r.rootContext.asInstanceOf
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    sym.is(dotc.core.Flags.Extension)
  }

  def hackIsOpaque(r: Reflection)(rsym: r.Symbol): Boolean = {
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = r.rootContext.asInstanceOf
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    sym.is(dotc.core.Flags.Opaque)
  }

  def hackIsLeftAssoc(d: Symbol): Boolean = !d.name.endsWith(":")

  def getExtendedSymbol(d: Symbol): Option[ValDef] = 
    Option.when(hackIsExtension(self.reflect)(d))(
      if(hackIsLeftAssoc(d)) d.tree.asInstanceOf[DefDef].paramss(0)(0)
      else d.tree.asInstanceOf[DefDef].paramss(1)(0)
    )

  object MatchTypeCase:
    def unapply(tpe: Type): Option[(TypeOrBounds, TypeOrBounds)] = 
      tpe match
        case AppliedType(t, Seq(from, to)) if t == MatchCaseType =>
            Some((from, to))
        case TypeLambda(paramNames, paramTypes, AppliedType(t, Seq(from, to))) if t == MatchCaseType =>
            Some((from, to))
        case _ =>
          None    
