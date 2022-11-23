package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeTraverser
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.ImportSelector
import dotty.tools.dotc.config.ScalaSettings
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.{em, i}
import dotty.tools.dotc.core.Flags.{Given, GivenVal, Param, Private, SelfName}
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.Message
import dotty.tools.dotc.typer.ImportInfo
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.transform.CheckUnused.UnusedData.UnusedResult
import dotty.tools.dotc.core.Mode



/**
 * A compiler phase that checks for unused imports or definitions
 *
 * Basically, it gathers definition/imports and their usage. If a
 * definition/imports does not have any usage, then it is reported.
 */
class CheckUnused extends Phase:
  import CheckUnused.UnusedData

  private val _key = Property.Key[UnusedData]

  override def phaseName: String = CheckUnused.phaseName

  override def description: String = CheckUnused.description

  override def isRunnable(using Context): Boolean =
    ctx.settings.Wunused.value.nonEmpty &&
    !ctx.isJava

  override def run(using Context): Unit =
    val tree = ctx.compilationUnit.tpdTree
    val data = UnusedData()
    val fresh = ctx.fresh.setProperty(_key, data)
    traverser.traverse(tree)(using fresh)
    reportUnused(data.getUnused)

  /**
   * This traverse is the **main** component of this phase
   *
   * It traverse the tree the tree and gather the data in the
   * corresponding context property
   */
  private def traverser = new TreeTraverser:
    import tpd._
    import UnusedData.ScopeType

    /* Register every imports, definition and usage */
    override def traverse(tree: tpd.Tree)(using Context): Unit =
      val unusedDataApply = ctx.property(_key).foreach
      val newCtx = if tree.symbol.exists then ctx.withOwner(tree.symbol) else ctx
      if tree.isDef then // register the annotations for usage
        unusedDataApply(_.registerUsedAnnotation(tree.symbol))
      tree match
        case imp:tpd.Import =>
          unusedDataApply(_.registerImport(imp))
        case ident: Ident =>
          unusedDataApply(_.registerUsed(ident.symbol))
          traverseChildren(tree)(using newCtx)
        case sel: Select =>
          unusedDataApply(_.registerUsed(sel.symbol))
          traverseChildren(tree)(using newCtx)
        case _: (tpd.Block | tpd.Template) =>
          unusedDataApply { ud =>
            ud.inNewScope(ScopeType.fromTree(tree))(traverseChildren(tree)(using newCtx))
          }
        case t:tpd.ValDef =>
          unusedDataApply(_.registerDef(t))
          traverseChildren(tree)(using newCtx)
        case t:tpd.DefDef =>
          unusedDataApply(_.registerDef(t))
          traverseChildren(tree)(using newCtx)
        case t: tpd.Bind =>
          unusedDataApply(_.registerPatVar(t))
          traverseChildren(tree)(using newCtx)
        case _ =>
          traverseChildren(tree)(using newCtx)
    end traverse
  end traverser

  private def reportUnused(res: UnusedData.UnusedResult)(using Context): Unit =
    import CheckUnused.WarnTypes
    res.warnings.foreach { s =>
      s match
        case (t, WarnTypes.Imports) =>
          report.warning(s"unused import", t)
        case (t, WarnTypes.LocalDefs) =>
          report.warning(s"unused local definition", t)
        case (t, WarnTypes.ExplicitParams) =>
          report.warning(s"unused explicit parameter", t)
        case (t, WarnTypes.ImplicitParams) =>
          report.warning(s"unused implicit parameter", t)
        case (t, WarnTypes.PrivateMembers) =>
          report.warning(s"unused private member", t)
        case (t, WarnTypes.PatVars) =>
          report.warning(s"unused pattern variable", t)
    }

end CheckUnused

object CheckUnused:
  val phaseName: String = "checkUnused"
  val description: String = "check for unused elements"

  enum WarnTypes:
    case Imports
    case LocalDefs
    case ExplicitParams
    case ImplicitParams
    case PrivateMembers
    case PatVars

  /**
   * A stateful class gathering the infos on :
   * - imports
   * - definitions
   * - usage
   */
  private class UnusedData:
    import collection.mutable.{Set => MutSet, Map => MutMap, Stack => MutStack}
    import dotty.tools.dotc.core.Symbols.Symbol
    import UnusedData.ScopeType

    /** The current scope during the tree traversal */
    var currScopeType: ScopeType = ScopeType.Other

    /* IMPORTS */
    private val impInScope = MutStack(MutSet[tpd.Import]())
    /**
     * We store the symbol along with their accessibility without import.
     * Accessibility to their definition in outer context/scope
     *
     * See the `isAccessibleAsIdent` extension method below in the file
     */
    private val usedInScope = MutStack(MutSet[(Symbol,Boolean)]())
    /* unused import collected during traversal */
    private val unusedImport = MutSet[ImportSelector]()

    /* LOCAL DEF OR VAL / Private Def or Val / Pattern variables */
    private val localDefInScope = MutSet[tpd.ValOrDefDef]()
    private val privateDefInScope = MutSet[tpd.ValOrDefDef]()
    private val explicitParamInScope = MutSet[tpd.ValOrDefDef]()
    private val implicitParamInScope = MutSet[tpd.ValOrDefDef]()
    private val patVarsInScope = MutSet[tpd.Bind]()

    /* Unused collection collected at the end */
    private val unusedLocalDef = MutSet[tpd.ValOrDefDef]()
    private val unusedPrivateDef = MutSet[tpd.ValOrDefDef]()
    private val unusedExplicitParams = MutSet[tpd.ValOrDefDef]()
    private val unusedImplicitParams = MutSet[tpd.ValOrDefDef]()
    private val unusedPatVars = MutSet[tpd.Bind]()

    /** All used symbols */
    private val usedDef = MutSet[Symbol]()

    /**
     * Push a new Scope of the given type, executes the given Unit and
     * pop it back to the original type.
     */
    def inNewScope(newScope: ScopeType)(execInNewScope: => Unit)(using Context): Unit =
      val prev = currScopeType
      currScopeType = newScope
      pushScope()
      execInNewScope
      popScope()
      currScopeType = prev

    /** Register all annotations of this symbol's denotation */
    def registerUsedAnnotation(sym: Symbol)(using Context): Unit =
      val annotSym = sym.denot.annotations.map(_.symbol)
      registerUsed(annotSym)

    /** Register a found (used) symbol */
    def registerUsed(sym: Symbol)(using Context): Unit =
      usedInScope.top += sym -> sym.isAccessibleAsIdent
      usedDef += sym

    /** Register a list of found (used) symbols */
    def registerUsed(syms: Seq[Symbol])(using Context): Unit =
      usedInScope.top ++= syms.map(s => s -> s.isAccessibleAsIdent)
      usedDef ++= syms

    /** Register an import */
    def registerImport(imp: tpd.Import)(using Context): Unit =
      val tpd.Import(qual, selectors) = imp
      if !tpd.languageImport(qual).nonEmpty then
        impInScope.top += imp
        unusedImport ++= selectors.filter{ s =>
          !isImportExclusion(s) && !s.isGiven && !isSelectorOnAGiven(qual, s)
        }

    /** Register (or not) some `val` or `def` according to the context, scope and flags */
    def registerDef(valOrDef: tpd.ValOrDefDef)(using Context): Unit =
      if valOrDef.symbol.is(Param) then
        if valOrDef.symbol.is(Given) then
          implicitParamInScope += valOrDef
        else
          explicitParamInScope += valOrDef
      else if currScopeType == ScopeType.Local then
        localDefInScope += valOrDef
      else if currScopeType == ScopeType.Template && valOrDef.symbol.is(Private, butNot = SelfName) then
        privateDefInScope += valOrDef

    /** Register pattern variable */
    def registerPatVar(patvar: tpd.Bind)(using Context): Unit =
      patVarsInScope += patvar

    /** enter a new scope */
    def pushScope(): Unit =
      // unused imports :
      impInScope.push(MutSet())
      usedInScope.push(MutSet())

    /**
     * leave the current scope and do :
     *
     * - If there are imports in this scope check for unused ones
     */
    def popScope()(using Context): Unit =
      // used symbol in this scope
      val used = usedInScope.pop().toSet
      // used imports in this scope
      val imports = impInScope.pop().toSet
      val kept = used.filter { t =>
        val (sym, isAccessible) = t
        // keep the symbol for outer scope, if it matches **no** import
        !imports.exists { imp =>
          sym.isInImport(imp, isAccessible) match
            case None => false
            case Some(sel) =>
              unusedImport -= sel
              true
        }
      }
      // if there's an outer scope
      if usedInScope.nonEmpty then
        // we keep the symbols not referencing an import in this scope
        // as it can be the only reference to an outer import
        usedInScope.top ++= kept
      // register usage in this scope for other warnings at the end of the phase
      usedDef ++= used.map(_._1)
    end popScope

    /**
     * Leave the scope and return a `List` of unused `ImportSelector`s
     *
     * The given `List` is sorted by line and then column of the position
     */
    def getUnused(using Context): UnusedResult =
      popScope()
      val sortedImp =
        if ctx.settings.WunusedHas.imports then
          unusedImport.map(d => d.srcPos -> WarnTypes.Imports).toList
        else
          Nil
      val sortedLocalDefs =
        if ctx.settings.WunusedHas.locals then
          localDefInScope.filter(d => !usedDef(d.symbol)).map(d => d.namePos -> WarnTypes.LocalDefs).toList
        else
          Nil
      val sortedExplicitParams =
        if ctx.settings.WunusedHas.explicits then
          explicitParamInScope.filter(d => !usedDef(d.symbol)).map(d => d.namePos -> WarnTypes.ExplicitParams).toList
        else
          Nil
      val sortedImplicitParams =
        if ctx.settings.WunusedHas.implicits then
          implicitParamInScope.filter(d => !usedDef(d.symbol)).map(d => d.namePos -> WarnTypes.ImplicitParams).toList
        else
          Nil
      val sortedPrivateDefs =
        if ctx.settings.WunusedHas.privates then
          privateDefInScope.filter(d => !usedDef(d.symbol)).map(d => d.namePos -> WarnTypes.PrivateMembers).toList
        else
          Nil
      val sortedPatVars =
        if ctx.settings.WunusedHas.patvars then
          patVarsInScope.filter(d => !usedDef(d.symbol)).map(d => d.namePos -> WarnTypes.PatVars).toList
        else
          Nil
      val warnings = List(sortedImp, sortedLocalDefs, sortedExplicitParams, sortedImplicitParams, sortedPrivateDefs, sortedPatVars).flatten.sortBy { s =>
        val pos = s._1.sourcePos
        (pos.line, pos.column)
      }
      UnusedResult(warnings, Nil)
    end getUnused
    //============================ HELPERS ====================================

    private def isSelectorOnAGiven(qual: tpd.Tree, sel: ImportSelector)(using Context): Boolean =
      qual.tpe.member(sel.name).alternatives.exists(_.symbol.is(Given))

    private def isImportExclusion(sel: ImportSelector): Boolean = sel.renamed match
      case untpd.Ident(name) => name == StdNames.nme.WILDCARD
      case _ => false

    extension (sym: Symbol)
      /** is accessible without import in current context */
      def isAccessibleAsIdent(using Context): Boolean =
        sym.exists &&
          ctx.outersIterator.exists{ c =>
            c.owner == sym.owner
            || sym.owner.isClass && c.owner.isClass
                && c.owner.thisType.baseClasses.contains(sym.owner)
                && c.owner.thisType.member(sym.name).alternatives.contains(sym)
          }

      /** Given an import and accessibility, return an option of selector that match import<->symbol */
      def isInImport(imp: tpd.Import, isAccessible: Boolean)(using Context): Option[ImportSelector] =
        val tpd.Import(qual, sels) = imp
        val qualHasSymbol = qual.tpe.member(sym.name).symbol == sym
        def selector = sels.find(sel => sel.name.toTermName == sym.name || sel.name.toTypeName == sym.name)
        def wildcard = sels.find(sel => sel.isWildcard && (sym.is(Given) == sel.isGiven))
        if qualHasSymbol && !isAccessible then
          selector.orElse(wildcard) // selector with name or wildcard (or given)
        else
          None

  end UnusedData

  object UnusedData:
      enum ScopeType:
        case Local
        case Template
        case Other

      object ScopeType:
        def fromTree(tree: tpd.Tree): ScopeType = tree match
          case _:tpd.Template => Template
          case _:tpd.Block => Local
          case _ => Other

      case class UnusedResult(warnings: List[(dotty.tools.dotc.util.SrcPos, WarnTypes)], usedImports: List[(tpd.Import, untpd.ImportSelector)])
end CheckUnused

