package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeTraverser
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.ImportSelector
import dotty.tools.dotc.config.ScalaSettings
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Flags.{Private, Given}
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.Message
import dotty.tools.dotc.typer.ImportInfo
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.transform.CheckUnused.UnusedData.UnusedResult

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
    ctx.settings.WunusedHas.privates

  override def run(using Context): Unit =
    val tree = ctx.compilationUnit.tpdTree
    val data = UnusedData(ctx)
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
      case imp@Import(_, sels) => sels.foreach { s =>
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
      case _ => traverseChildren(tree)

  }

  private def reportUnused(res: UnusedData.UnusedResult)(using Context) =
    val UnusedData.UnusedResult(imports, locals, privates) = res
    /* IMPORTS */
    if ctx.settings.WunusedHas.imports then
      imports.foreach { s =>
        report.warning(i"unused import", s.srcPos)
      }
    /* LOCAL VAL OR DEF */
    if ctx.settings.WunusedHas.locals then
      locals.foreach { s =>
        report.warning(i"unused local definition", s.srcPos)
      }
    /* PRIVATES VAL OR DEF */
    if ctx.settings.WunusedHas.privates then
      privates.foreach { s =>
        report.warning(i"unused private member", s.srcPos)
      }

end CheckUnused

object CheckUnused:
  val phaseName: String = "checkUnused"
  val description: String = "check for unused elements"

  /**
   * A stateful class gathering the infos on :
   * - imports
   * - definitions
   * - usage
   */
  private class UnusedData(initctx: Context):
    import collection.mutable.{Set => MutSet, Map => MutMap, Stack, ListBuffer}
    import UnusedData.ScopeType

    var currScope: ScopeType = ScopeType.Other

    /* IMPORTS */
    private val impInScope = Stack(MutMap[Int, ListBuffer[ImportSelector]]())
    private val unusedImport = ListBuffer[ImportSelector]()
    private val usedImports = Stack(MutSet[Int]())

    /* LOCAL DEF OR VAL / Private Def or Val*/
    private val localDefInScope = Stack(MutSet[tpd.ValOrDefDef]())
    private val privateDefInScope = Stack(MutSet[tpd.ValOrDefDef]())
    private val unusedLocalDef = ListBuffer[tpd.ValOrDefDef]()
    private val unusedPrivateDef = ListBuffer[tpd.ValOrDefDef]()
    private val usedDef = MutSet[Int]()

    private def isImportExclusion(sel: ImportSelector): Boolean = sel.renamed match
      case ident@untpd.Ident(name) => name == StdNames.nme.WILDCARD
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
      if currScope == ScopeType.Local then
        localDefInScope.top += valOrDef
      else if currScope == ScopeType.Template && valOrDef.symbol.is(Private) then
        privateDefInScope.top += valOrDef

    /** enter a new scope */
    def pushScope(): Unit =
      // unused imports :
      usedImports.push(MutSet())
      impInScope.push(MutMap())
      // local and private defs :
      localDefInScope.push(MutSet())
      privateDefInScope.push(MutSet())

    /** leave the current scope */
    def popScope()(using Context): Unit =
      popScopeImport()
      popScopeLocalDef()
      popScopePrivateDef()

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
      if usedImports.size > 0 then
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

    def popScopePrivateDef()(using Context): Unit =
      val unused = privateDefInScope.pop().filterInPlace{d =>
        !usedDef(d.symbol.id)
      }
      unusedPrivateDef ++= unused

    /**
     * Leave the scope and return a `List` of unused `ImportSelector`s
     *
     * The given `List` is sorted by line and then column of the position
     */
    def getUnused(using Context): UnusedResult =
      popScope()
      val sortedImp = unusedImport.toList.sortBy{ sel =>
        val pos = sel.srcPos.sourcePos
        (pos.line, pos.column)
      }
      val sortedLocalDefs = unusedLocalDef.toList.sortBy { sel =>
        val pos = sel.srcPos.sourcePos
        (pos.line, pos.column)
      }
      val sortedPrivateDefs = unusedPrivateDef.toList.sortBy { sel =>
        val pos = sel.srcPos.sourcePos
        (pos.line, pos.column)
      }
      UnusedResult(sortedImp, sortedLocalDefs, sortedPrivateDefs)

  end UnusedData

  object UnusedData:
      enum ScopeType:
        case Local
        case Template
        case Other

      case class UnusedResult(
        imports: List[ImportSelector],
        locals: List[tpd.ValOrDefDef],
        privates: List[tpd.ValOrDefDef],
      )
end CheckUnused

