package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeTraverser
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.ImportSelector
import dotty.tools.dotc.config.ScalaSettings
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.{em, i}
import dotty.tools.dotc.core.Flags.{Given, GivenVal, Param, Private}
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.Message
import dotty.tools.dotc.typer.ImportInfo
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.transform.CheckUnused.UnusedData.UnusedResult
import dotty.tools.dotc.core.Mode

import scala.collection.mutable


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
    ctx.settings.WunusedHas.imports ||
    ctx.settings.WunusedHas.locals  ||
    ctx.settings.WunusedHas.explicits  ||
    ctx.settings.WunusedHas.implicits  ||
    ctx.settings.WunusedHas.privates ||
    ctx.settings.WunusedHas.patvars

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
  private def traverser = new TreeTraverser {
    import tpd._
    import UnusedData.ScopeType

    override def traverse(tree: tpd.Tree)(using Context): Unit = tree match
      case imp@Import(_, sels) => sels.foreach { _ =>
          ctx.property(_key).foreach(_.registerImport(imp))
        }
      case ident: Ident =>
        val id = ident.symbol.id
        ctx.property(_key).foreach(_.registerUsed(id))
        traverseChildren(tree)
      case sel: Select =>
        val id = sel.symbol.id
        ctx.property(_key).foreach(_.registerUsed(id))
        traverseChildren(tree)
      case tpd.Block(_,_) =>
        var prev: ScopeType = ScopeType.Other
        ctx.property(_key).foreach { ud =>
          ud.pushScope()
          prev = ud.currScope
          ud.currScope = ScopeType.Local
        }
        traverseChildren(tree)
        ctx.property(_key).foreach { ud =>
          ud.popScope()
          ud.currScope = prev
        }
      case tpd.Template(_,_,_,_) =>
        var prev: ScopeType = ScopeType.Other
        ctx.property(_key).foreach { ud =>
          ud.pushScope()
          prev = ud.currScope
          ud.currScope = ScopeType.Template
        }
        traverseChildren(tree)
        ctx.property(_key).foreach { ud =>
          ud.popScope()
          ud.currScope = prev
        }
      case t:tpd.ValDef =>
        ctx.property(_key).foreach(_.registerDef(t))
        traverseChildren(tree)
      case t:tpd.DefDef =>
        ctx.property(_key).foreach(_.registerDef(t))
        traverseChildren(tree)
      case t: tpd.Bind =>
        ctx.property(_key).foreach(_.registerPatVar(t))
        traverseChildren(tree)
      case _ => traverseChildren(tree)

  }

  private def reportUnused(res: UnusedData.UnusedResult)(using Context): Unit =
    import CheckUnused.WarnTypes
    res.foreach { s =>
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
    import collection.mutable.{Set => MutSet, Map => MutMap, Stack => MutStack, ListBuffer}
    import UnusedData.ScopeType

    var currScope: ScopeType = ScopeType.Other

    /* IMPORTS */
    private val impInScope = MutStack(MutMap[Int, ListBuffer[ImportSelector]]())
    private val unusedImport = MutSet[ImportSelector]()
    private val usedImports = MutStack(MutSet[Int]())

    /* LOCAL DEF OR VAL / Private Def or Val / Pattern variables */
    private val localDefInScope = MutStack(MutSet[tpd.ValOrDefDef]())
    private val privateDefInScope = MutStack(MutSet[tpd.ValOrDefDef]())
    private val explicitParamInScope = MutStack(MutSet[tpd.ValOrDefDef]())
    private val implicitParamInScope = MutStack(MutSet[tpd.ValOrDefDef]())
    private val patVarsInScope = MutStack(MutSet[tpd.Bind]())

    private val unusedLocalDef = ListBuffer[tpd.ValOrDefDef]()
    private val unusedPrivateDef = ListBuffer[tpd.ValOrDefDef]()
    private val unusedExplicitParams = ListBuffer[tpd.ValOrDefDef]()
    private val unusedImplicitParams = ListBuffer[tpd.ValOrDefDef]()
    private val unusedPatVars = ListBuffer[tpd.Bind]()

    private val usedDef = MutSet[Int]()

    private def isImportExclusion(sel: ImportSelector): Boolean = sel.renamed match
      case untpd.Ident(name) => name == StdNames.nme.WILDCARD
      case _ => false

    /** Register the id of a found (used) symbol */
    def registerUsed(id: Int): Unit =
      usedImports.top += id
      usedDef += id

    /** Register an import */
    def registerImport(imp: tpd.Import)(using Context): Unit =
      val tpd.Import(tree, sels) = imp
      val map = impInScope.top
      val entries = sels.flatMap{ s =>
        if isImportExclusion(s) then
          Nil // ignore exclusion import
        else if s.isWildcard then
          tree.tpe.allMembers
            .filter(m => m.symbol.is(Given) == s.isGiven) // given imports
            .map(_.symbol.id -> s)

        else
          val id = tree.tpe.member(s.name.toTermName).symbol.id
          val typeId = tree.tpe.member(s.name.toTypeName).symbol.id
          List(id -> s, typeId -> s)
      }
      entries.foreach{(id, sel) =>
        map.get(id) match
          case None => map.put(id, ListBuffer(sel))
          case Some(value) => value += sel
      }

    def registerDef(valOrDef: tpd.ValOrDefDef)(using Context): Unit =
      if valOrDef.symbol.is(Param) then
        if valOrDef.symbol.is(Given) then
          implicitParamInScope.top += valOrDef
        else
          explicitParamInScope.top += valOrDef
      else if currScope == ScopeType.Local then
        localDefInScope.top += valOrDef
      else if currScope == ScopeType.Template && valOrDef.symbol.is(Private) then
        privateDefInScope.top += valOrDef

    def registerPatVar(patvar: tpd.Bind)(using Context): Unit =
      patVarsInScope.top += patvar

    /** enter a new scope */
    def pushScope(): Unit =
      // unused imports :
      usedImports.push(MutSet())
      impInScope.push(MutMap())
      // local and private defs :
      localDefInScope.push(MutSet())
      explicitParamInScope.push(MutSet())
      implicitParamInScope.push(MutSet())
      privateDefInScope.push(MutSet())
      patVarsInScope.push(MutSet())

    /** leave the current scope */
    def popScope()(using Context): Unit =
      popScopeImport()
      popScopeLocalDef()
      popScopeExplicitParam()
      popScopeImplicitParam()
      popScopePrivateDef()
      popScopePatVars()

    def popScopeImport(): Unit =
      val usedImp = MutSet[ImportSelector]()
      val poppedImp = impInScope.pop()
      val notDefined = usedImports.pop.filter{id =>
        poppedImp.remove(id) match
          case None => true
          case Some(value) =>
            usedImp.addAll(value)
            false
      }
      if usedImports.nonEmpty then
        usedImports.top.addAll(notDefined)

      poppedImp.values.flatten.foreach{ sel =>
        // If **any** of the entities used by the import is used,
        // do not add to the `unused` Set
        if !usedImp(sel) then
          unusedImport += sel
      }

    def popScopeLocalDef()(using Context): Unit =
      val unused = localDefInScope.pop().filterInPlace(d => !usedDef(d.symbol.id))
      unusedLocalDef ++= unused

    def popScopeExplicitParam()(using Context): Unit =
      val unused = explicitParamInScope.pop().filterInPlace(d => !usedDef(d.symbol.id))
      unusedExplicitParams ++= unused

    def popScopeImplicitParam()(using Context): Unit =
      val unused = implicitParamInScope.pop().filterInPlace(d => !usedDef(d.symbol.id))
      unusedImplicitParams ++= unused

    def popScopePrivateDef()(using Context): Unit =
      val unused = privateDefInScope.pop().filterInPlace(d => !usedDef(d.symbol.id))
      unusedPrivateDef ++= unused

    def popScopePatVars()(using Context): Unit =
      val unused = patVarsInScope.pop().filterInPlace(d => !usedDef(d.symbol.id))
      unusedPatVars ++= unused

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
          unusedLocalDef.map(d => d.namePos -> WarnTypes.LocalDefs).toList
        else
          Nil
      val sortedExplicitParams =
        if ctx.settings.WunusedHas.explicits then
          unusedExplicitParams.map(d => d.namePos -> WarnTypes.ExplicitParams).toList
        else
          Nil
      val sortedImplicitParams =
        if ctx.settings.WunusedHas.implicits then
          unusedImplicitParams.map(d => d.namePos -> WarnTypes.ImplicitParams).toList
        else
          Nil
      val sortedPrivateDefs =
        if ctx.settings.WunusedHas.privates then
          unusedPrivateDef.map(d => d.namePos -> WarnTypes.PrivateMembers).toList
        else
          Nil
      val sortedPatVars =
        if ctx.settings.WunusedHas.patvars then
          unusedPatVars.map(d => d.namePos -> WarnTypes.PatVars).toList
        else
          Nil
      List(sortedImp, sortedLocalDefs, sortedExplicitParams, sortedImplicitParams, sortedPrivateDefs, sortedPatVars).flatten.sortBy { s =>
        val pos = s._1.sourcePos
        (pos.line, pos.column)
      }

  end UnusedData

  object UnusedData:
      enum ScopeType:
        case Local
        case Template
        case Param
        case Other

      type UnusedResult = List[(dotty.tools.dotc.util.SrcPos, WarnTypes)]
end CheckUnused

