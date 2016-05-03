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

object Phases {

  class DocPhase extends Phase {
    import model.comment.Comment
    import model.CommentParsers.wikiParser
    import model.factories._
    import model._
    import model.internal._
    import dotty.tools.dotc.core.Flags
    import dotty.tools.dotc.ast.tpd._
    import util.traversing._
    import util.internal.setters._

    def phaseName = "docphase"

    private[this] var commentCache: Map[String, (Entity, Map[String, Package]) => Option[Comment]] = Map.empty

    /** Saves the commentParser function for later evaluation, for when the AST has been filled */
    def track(symbol: Symbol, ctx: Context)(op: => Entity) = {
      val entity = op

      if (entity != NonEntity) {
        val commentParser = { (entity: Entity, packs: Map[String, Package]) =>
          wikiParser.parseHtml(symbol, entity, packs)(ctx)
        }

        commentCache = commentCache + (entity.path.mkString(".") -> commentParser)
      }
      entity
    }

    /** Build documentation hierarchy from existing tree */
    def collect(tree: Tree, prev: List[String] = Nil)(implicit ctx: Context): Entity = track(tree.symbol, ctx) {

      def collectList(xs: List[Tree], ps: List[String])(implicit ctx: Context): List[Entity] =
        xs.map(collect(_, ps)).filter(_ != NonEntity)

      def collectEntityMembers(xs: List[Tree], ps: List[String])(implicit ctx: Context) =
        collectList(xs, ps).asInstanceOf[List[Entity with Members]]

      def collectMembers(tree: Tree, ps: List[String] = prev)(implicit ctx: Context): List[Entity] = tree match {
        case t: Template => collectList(t.body, ps)
        case _ => Nil
      }

      tree match {
        /** package */
        case pd @ PackageDef(pid, st) =>
          val newPath = prev :+ pid.name.toString
          addEntity(PackageImpl(newPath.mkString("."), collectEntityMembers(st, newPath), newPath))

        /** trait */
        case t @ TypeDef(n, rhs) if t.symbol.is(Flags.Trait) =>
          val name = n.toString
          val newPath = prev :+ name
          TraitImpl(name, collectMembers(rhs), flags(t), newPath)

        /** objects, on the format "Object$" so drop the last letter */
        case o @ TypeDef(n, rhs) if o.symbol.is(Flags.Module) =>
          val name = n.toString.dropRight(1)
          ObjectImpl(name, collectMembers(rhs, prev :+ name),  flags(o), prev :+ (name + "$"))

        /** class / case class */
        case c @ TypeDef(name, rhs) if c.symbol.isClass =>
          val newPath = prev :+ name.toString
          (name.toString, collectMembers(rhs), flags(c), newPath, None) match {
            case x if c.symbol.is(Flags.CaseClass) => CaseClassImpl.tupled(x)
            case x => ClassImpl.tupled(x)
          }

        /** def */
        case d: DefDef =>
          DefImpl(d.name.toString, flags(d), path(d), returnType(d.tpt))

        /** val */
        case v: ValDef if !v.symbol.is(Flags.ModuleVal) =>
          ValImpl(v.name.toString, flags(v), path(v), returnType(v.tpt))

        case x => {
          //dottydoc.println(s"Found unwanted entity: $x (${x.pos},\n${x.show}")
          NonEntity
        }
      }
    }

    var packages: Map[String, Package] = Map.empty

    def addEntity(p: Package): Package = {
      val path    = p.path.mkString(".")
      val newPack = packages.get(path).map { ex =>
        val children = (ex.children ::: p.children).distinct.sortBy(_.name)
        PackageImpl(p.name, children, p.path, None)
      }.getOrElse(p)

      packages = packages + (path -> newPack)
      newPack
    }

    private[this] var totalRuns  = 0
    private[this] var currentRun = 0

    override def run(implicit ctx: Context): Unit = {
      currentRun += 1
      println(s"Generating schema for ($currentRun/$totalRuns): ${ctx.compilationUnit.source.file.name}")
      collect(ctx.compilationUnit.tpdTree) // Will put packages in `packages` var
    }

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
      // (1) Create package structure for all `units`, this will give us a complete structure
      totalRuns = units.length
      val compUnits = super.runOn(units)

      // (2) Set parent of all package children
      println("Connecting parents to children, finding companions...")
      for {
        parent <- packages.values
        child  <- parent.children
      } setParent(child, to = parent)

      // (3) Create documentation template from docstrings, with internal links
      for {
        pack <- packages.values
      } mutateEntities(pack) { e =>
        val comment = commentCache(e.path.mkString("."))(e, packages)
        setComment(e, to = comment)
      }

      // (4) Write the finished model to JSON
      util.IndexWriters.writeJs(packages, "../js/out")

      // (5) Clear caches
      commentCache = Map.empty

      // Return super's result
      compUnits
    }
  }

}
