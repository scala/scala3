package dotty.tools
package dottydoc
package core

/** Dotty and Dottydoc imports */
import dotc.ast.Trees._
import dotc.CompilationUnit
import dotc.core.Contexts.Context
import dotc.core.Types.PolyType
import dotc.core.Phases.Phase
import dotc.core.Symbols.{ Symbol, NoSymbol }
import dotc.core.NameOps._

class DocASTPhase extends Phase {
  import model._
  import model.factories._
  import model.internal._
  import dotty.tools.dotc.core.Flags
  import dotty.tools.dotc.ast.tpd._
  import dotty.tools.dottydoc.util.syntax._
  import util.traversing._
  import util.internal.setters._

  def phaseName = "docASTPhase"

  /** Build documentation hierarchy from existing tree */
  def collect(tree: Tree)(implicit ctx: Context): List[Entity] = {
    val implicitConversions = ctx.docbase.defs(tree.symbol)

    def collectList(xs: List[Tree]): List[Entity] =
      xs.flatMap(collect)

    def collectEntityMembers(xs: List[Tree]) =
      collectList(xs).asInstanceOf[List[Entity with Members]]

    def collectMembers(tree: Tree)(implicit ctx: Context): List[Entity] = {
      val defs = tree match {
        case t: Template => collectList(t.body)
        case _ => Nil
      }

      defs ++ implicitConversions.flatMap(membersFromSymbol)
    }

    def membersFromSymbol(sym: Symbol): List[Entity] = {
      if (sym.info.exists) {
        val defs = sym.info.bounds.hi.finalResultType.membersBasedOnFlags(Flags.allOf(Flags.Method), Flags.Synthetic | Flags.Private)
          .filterNot(_.symbol.owner.name.show == "Any")
          .map { meth =>
            DefImpl(
              meth.symbol,
              annotations(meth.symbol),
              meth.symbol.name.show,
              Nil,
              path(meth.symbol),
              returnType(meth.info),
              typeParams(meth.symbol),
              paramLists(meth.info),
              implicitlyAddedFrom = Some(returnType(meth.symbol.owner.info))
            )
          }.toList

        // don't add privates, synthetics or class parameters (i.e. value class constructor val)
        val vals = sym.info.fields.filterNot(_.symbol.isOneOf(Flags.ParamAccessor | Flags.Private | Flags.Synthetic)).map { value =>
          val kind = if (value.symbol.is(Flags.Mutable)) "var" else "val"
          ValImpl(
            value.symbol,
            annotations(value.symbol),
            value.symbol.name.show,
            Nil, path(value.symbol),
            returnType(value.info),
            kind,
            implicitlyAddedFrom = Some(returnType(value.symbol.owner.info))
          )
        }

        defs ++ vals
      }
      else Nil
    }


    if (tree.symbol.is(Flags.Synthetic) && !tree.symbol.is(Flags.Module)) Nil
    else tree match {
      /** package */
      case pd @ PackageDef(pid, st) =>
        addPackage(PackageImpl(pd.symbol, annotations(pd.symbol), pd.symbol.showFullName, collectEntityMembers(st), path(pd.symbol))) :: Nil

      /** type alias */
      case t: TypeDef if !t.isClassDef =>
        val sym = t.symbol
        if (sym.isOneOf(Flags.Synthetic | Flags.Param))
          Nil
        else {
          val tparams = t.rhs.tpe match {
            case tp: PolyType => tp.paramNames.map(_.show)
            case _ => Nil
          }
          TypeAliasImpl(sym, annotations(sym), flags(t), t.name.show.split("\\$\\$").last, path(sym), alias(t.rhs.tpe), tparams) :: Nil
        }

      /** trait */
      case t @ TypeDef(n, rhs) if t.symbol.is(Flags.Trait) =>
        //TODO: should not `collectMember` from `rhs` - instead: get from symbol, will get inherited members as well
        TraitImpl(t.symbol, annotations(t.symbol), n.show, collectMembers(rhs), flags(t), path(t.symbol), typeParams(t.symbol), traitParameters(t.symbol), superTypes(t)) :: Nil

      /** objects, on the format "Object$" so drop the last letter */
      case o @ TypeDef(n, rhs) if o.symbol.is(Flags.Module) =>
        //TODO: should not `collectMember` from `rhs` - instead: get from symbol, will get inherited members as well
        ObjectImpl(o.symbol, annotations(o.symbol), o.name.stripModuleClassSuffix.show, collectMembers(rhs),  flags(o), path(o.symbol), superTypes(o)) :: Nil

      /** class / case class */
      case c @ TypeDef(n, rhs) if c.symbol.isClass =>
        //TODO: should not `collectMember` from `rhs` - instead: get from symbol, will get inherited members as well
        val parameters = (c.symbol, annotations(c.symbol), n.show, collectMembers(rhs), flags(c), path(c.symbol), typeParams(c.symbol), constructors(c.symbol), superTypes(c), None, Nil, None)
        if (c.symbol.is(Flags.CaseClass)) {
          CaseClassImpl.tupled(parameters) :: Nil
        } else {
          ClassImpl.tupled(parameters) :: Nil
        }

      /** def */
      case d: DefDef =>
        DefImpl(d.symbol, annotations(d.symbol), d.name.decode.toString, flags(d), path(d.symbol), returnType(d.tpt.tpe), typeParams(d.symbol), paramLists(d.symbol.info)) :: Nil

      /** val */
      case v: ValDef if !v.symbol.is(Flags.ModuleVal) =>
        val kind = if (v.symbol.is(Flags.Mutable)) "var" else "val"
        ValImpl(v.symbol, annotations(v.symbol), v.name.decode.toString, flags(v), path(v.symbol), returnType(v.tpt.tpe), kind) :: Nil

      case x => {
        ctx.docbase.debug(s"Found unwanted entity: $x (${x.span},\n${x.show}")
        Nil
      }
    }
  }

  var packages: Map[String, PackageImpl] = Map.empty

  def addPackage(newPkg: PackageImpl): Package = {
    def mergeMembers(newPkg: PackageImpl, oldPkg: PackageImpl): Unit = {
      val othersNew  = newPkg.members.filterNot(_.kind == "package")
      val (oldPacks, othersOld) = oldPkg.members.partition(_.kind == "package")

      val others = othersNew ::: othersOld
      // here we can just choose the old packs, since we're recursively (bottom up)
      // discovering the tree, we should have met the child packages first, as
      // such - they were already inserted into the tree
      val newMembers = (others ++ oldPacks)

      oldPkg.members = newMembers
    }

    // This function mutates packages in place as not to create any orphaned references
    def mergedPackages(old: PackageImpl, newPkg: PackageImpl): PackageImpl = {
      if (old.symbol eq NoSymbol) old.symbol = newPkg.symbol
      if (old.annotations.isEmpty) old.annotations = newPkg.annotations
      mergeMembers(newPkg, old)
      if (old.superTypes.isEmpty) old.superTypes = newPkg.superTypes
      if (old.comment.isEmpty) old.comment = newPkg.comment
      old
    }

    def insertOrModifyRoot(): PackageImpl = {
      val modifiedPkg =
        packages
        .get(newPkg.name)
        .map(mergedPackages(_, newPkg))
        .getOrElse(newPkg)

      packages = packages + (modifiedPkg.name -> modifiedPkg)
      modifiedPkg
    }

    // This function inserts a package by creating empty packages to the point
    // where it can insert the supplied package `newPkg`.
    def createAndInsert(currentPkg: PackageImpl, path: List[String]): PackageImpl = {
      (path: @unchecked) match {
        case x :: Nil => {
          val existingPkg = currentPkg.members.collectFirst {
            case p: PackageImpl if p.name == newPkg.name => p
          }

          if (existingPkg.isDefined) mergedPackages(existingPkg.get, newPkg)
          else {
            currentPkg.members = newPkg :: currentPkg.members
            newPkg
          }
        }
        case x :: xs => {
          val subPkg = s"${currentPkg.name}.$x"
          val existingPkg = currentPkg.members.collectFirst {
            case p: PackageImpl if p.name == subPkg => p
          }

          if (existingPkg.isDefined) createAndInsert(existingPkg.get, xs)
          else {
            val newEmpty = EmptyPackage(currentPkg.path :+ x, subPkg)
            packages = packages + (subPkg -> newEmpty)
            currentPkg.members = newEmpty :: currentPkg.members
            createAndInsert(newEmpty, xs)
          }
        }
      }
    }

    val path = newPkg.path
    if (path.length == 1)
      insertOrModifyRoot()
    else if (packages.contains(newPkg.name))
      mergedPackages(packages(newPkg.name), newPkg)
    else {
      val root = packages.get(path.head)
      if (root.isDefined)
        // Root ancestor of `newPkg` exists, start recursing to point of
        // insertion. Point of insertion will be the parent package of `newPkg`.
        //
        // Which is the first element of `newPkg`'s path - thus we use the tail
        // to continue traversing down the tree.
        createAndInsert(root.get, path.tail)
      else {
        val newEmpty = EmptyPackage(List(path.head), path.head)
        packages = packages + (path.head -> newEmpty)
        createAndInsert(newEmpty, path.tail)
      }
    }
  }

  private[this] var totalRuns  = 0
  private[this] var currentRun = 0

  override def run(implicit ctx: Context): Unit = {
    currentRun += 1
    ctx.echo(s"Compiling ($currentRun/$totalRuns): ${ctx.compilationUnit.source.file.name}")
    collect(ctx.compilationUnit.tpdTree) // Will put packages in `packages` var
  }

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    // (1) Create package structure for all `units`, this will give us a complete structure
    totalRuns = units.length
    val compUnits = super.runOn(units)

    // (2) Set parents of entities, needed for linking
    for {
      parent <- rootPackages(packages)
      child  <- parent.members
    } setParent(child, to = parent)

    // (3) Update Doc AST in ctx.base
    for (kv <- packages) ctx.docbase.packagesMutable += kv

    // Return super's result
    compUnits
  }
}
