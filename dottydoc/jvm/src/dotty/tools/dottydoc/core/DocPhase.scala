package dotty.tools
package dottydoc
package core

/** Dotty and Dottydoc imports */
import dotc.ast.Trees._
import dotc.CompilationUnit
import dotc.config.Printers.dottydoc
import dotc.core.Contexts.Context
import dotc.core.Phases.Phase
import dotc.core.Symbols.Symbol

class DocPhase extends Phase {
  import model.parsers.{ WikiParser, ReturnTypeParser }
  import model._
  import model.factories._
  import model.internal._
  import model.comment.Comment
  import dotty.tools.dotc.core.Flags
  import dotty.tools.dotc.ast.tpd._
  import util.traversing._
  import util.internal.setters._

  def phaseName = "docphase"

  private[this] val commentParser = new WikiParser
  private[this] val returnLinker  = new ReturnTypeParser

  /** Saves the commentParser function for later evaluation, for when the AST has been filled */
  def track(symbol: Symbol, ctx: Context)(op: => Entity) = {
    val entity = op

    if (entity != NonEntity)
      commentParser += (entity, symbol, ctx)

    entity
  }

  /** Build documentation hierarchy from existing tree */
  def collect(tree: Tree, prev: List[String] = Nil)(implicit ctx: Context): Entity = track(tree.symbol, ctx) {
    val implicitlyAddedMembers = ImplicitlyAdded.defs(tree.symbol)//denot.info)

    def collectList(xs: List[Tree], ps: List[String])(implicit ctx: Context): List[Entity] =
      xs.map(collect(_, ps)).filter(_ != NonEntity)

    def collectEntityMembers(xs: List[Tree], ps: List[String])(implicit ctx: Context) =
      collectList(xs, ps).asInstanceOf[List[Entity with Members]]

    /** TODO: should be a set, not a uniqued list */
    def collectMembers(tree: Tree, ps: List[String] = prev)(implicit ctx: Context): List[Entity] = {
      val defs = (tree match {
        case t: Template => collectList(t.body, ps)
        case _ => Nil
      }) ++ implicitlyAddedMembers.flatMap(defFromSymbol)

      defs
    }

    def defFromSymbol(sym: Symbol): List[Entity] = {
      val defs = sym.info.membersBasedOnFlags(Flags.Method, Flags.Synthetic | Flags.Private).map { meth =>
        track(sym, ctx) {
          println(meth.show)
          DefImpl(meth.symbol.name.decode.toString, Nil, path(meth.symbol), returnType(meth.info), typeParams(meth.symbol), Nil/*paramLists(???)*/)
        }
      }.toList

      println(s"Implicitly added to ${tree.symbol.show}:")
      defs.foreach(println)
      println("-------")
      defs
    }


    tree match {
      /** package */
      case pd @ PackageDef(pid, st) =>
        val newPath = prev :+ pid.name.toString
        addEntity(PackageImpl(newPath.mkString("."), collectEntityMembers(st, newPath), newPath))

      /** trait */
      case t @ TypeDef(n, rhs) if t.symbol.is(Flags.Trait) =>
        val name = n.decode.toString
        val newPath = prev :+ name
        //TODO: should not `collectMember` from `rhs` - instead: get from symbol, will get inherited members as well
        TraitImpl(name, collectMembers(rhs), flags(t), newPath, typeParams(t.symbol), superTypes(t))

      /** objects, on the format "Object$" so drop the last letter */
      case o @ TypeDef(n, rhs) if o.symbol.is(Flags.Module) =>
        val name = n.decode.toString.dropRight(1)
        //TODO: should not `collectMember` from `rhs` - instead: get from symbol, will get inherited members as well
        ObjectImpl(name, collectMembers(rhs, prev :+ name),  flags(o), prev :+ (name + "$"), superTypes(o))

      /** class / case class */
      case c @ TypeDef(n, rhs) if c.symbol.isClass =>
        val name = n.decode.toString
        val newPath = prev :+ name
        //TODO: should not `collectMember` from `rhs` - instead: get from symbol, will get inherited members as well
        (name, collectMembers(rhs), flags(c), newPath, typeParams(c.symbol), superTypes(c), None) match {
          case x if c.symbol.is(Flags.CaseClass) => CaseClassImpl.tupled(x)
          case x => ClassImpl.tupled(x)
        }

      /** def */
      case d: DefDef =>
        DefImpl(d.name.decode.toString, flags(d), path(d.symbol), returnType(d.tpt.tpe), typeParams(d.symbol), paramLists(d))

      /** val */
      case v: ValDef if !v.symbol.is(Flags.ModuleVal) =>
        ValImpl(v.name.decode.toString, flags(v), path(v.symbol), returnType(v.tpt.tpe))

      case x => {
        //dottydoc.println(s"Found unwanted entity: $x (${x.pos},\n${x.show}")
        NonEntity
      }
    }
  }

  var packages: Map[String, Package] = Map.empty

  def addEntity(p: Package): Package = {
    def mergedChildren(x1s: List[Entity], x2s: List[Entity]): List[Entity] = {
      val (packs1, others1) = x1s.partition(_.kind == "package")
      val (packs2, others2) = x2s.partition(_.kind == "package")

      val others = others1 ::: others2
      val packs  = (packs1 ::: packs2).groupBy(_.path).map(_._2.head)

      (others ++ packs).sortBy(_.name)
    }

    val path    = p.path.mkString(".")
    val newPack = packages.get(path).map {
      case ex: PackageImpl =>
        if (!ex.comment.isDefined) ex.comment = p.comment
        ex.members = mergedChildren(ex.members, p.members)
        ex
    }.getOrElse(p)

    packages = packages + (path -> newPack)
    newPack
  }

  private[this] var totalRuns  = 0
  private[this] var currentRun = 0

  override def run(implicit ctx: Context): Unit = {
    currentRun += 1
    println(s"Compiling ($currentRun/$totalRuns): ${ctx.compilationUnit.source.file.name}")
    collect(ctx.compilationUnit.tpdTree) // Will put packages in `packages` var
  }

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    // (1) Create package structure for all `units`, this will give us a complete structure
    totalRuns = units.length
    val compUnits = super.runOn(units)

    // (2) Set parents of entities, needed for linking
    for {
      parent <- packages.values
      child  <- parent.children
    } setParent(child, to = parent)


    // (3) Set returntypes to correct entities
    returnLinker.link(packages)

    // (3) Create documentation template from docstrings, with internal links
    println("Generating documentation, this might take a while...")
    commentParser.parse(packages)

    // (4) Write the finished model to JSON
    val outputDir = {
      val out = ctx.settings.DocOutput.value
      if (out.last == '/') out.dropRight(1)
      else out
    }
    if (!ctx.settings.YDocNoWrite.value) (new util.OutputWriter).write(packages, outputDir)

    // (5) Clear caches
    commentParser.clear()

    // Return super's result
    compUnits
  }
}
