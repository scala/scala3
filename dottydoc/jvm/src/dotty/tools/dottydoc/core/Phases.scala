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
    import model.Entities._
    import model.EntityFactories._
    import dotty.tools.dotc.core.Flags
    import dotty.tools.dotc.ast.tpd._

    def phaseName = "docphase"

    /** Build documentation hierarchy from existing tree */
    def collect(tree: Tree)(implicit ctx: Context): Entity = {

      def collectList(xs: List[Tree])(implicit ctx: Context): List[Entity] =
        xs.map(collect).filter(_ != NonEntity)

      def collectPackageMembers(xs: List[Tree])(implicit ctx: Context): List[PackageMember] =
        collectList(xs).asInstanceOf[List[PackageMember]]

      def collectMembers(tree: Tree)(implicit ctx: Context): List[Entity] = tree match {
        case t: Template => collectList(t.body)
        case _ => Nil
      }

      val comment = wikiParser.parseHtml(tree.symbol)

      tree match {
        /** package */
        case p @ PackageDef(pid, st) =>
          val name = pid.name.toString
          Package(name, collectPackageMembers(st), comment, path(p, name))

        /** trait */
        case t @ TypeDef(n, rhs) if t.symbol.is(Flags.Trait) =>
          val name = n.toString
          Trait(name, collectMembers(rhs), comment, flags(t), path(t, name))

        /** objects, on the format "Object$" so drop the last letter */
        case o @ TypeDef(n, rhs) if o.symbol.is(Flags.Module) =>
          val name = n.toString.dropRight(1)
          Object(name, collectMembers(rhs), comment, flags(o), path(o, name))

        /** class / case class */
        case c @ TypeDef(name, rhs) if c.symbol.isClass =>
          (name.toString, collectMembers(rhs), comment, flags(c), path(c, name.toString)) match {
            case x if c.symbol.is(Flags.CaseClass) => CaseClass.tupled(x)
            case x => Class.tupled(x)
          }

        /** def */
        case d: DefDef =>
          Def(d.name.toString, comment, flags(d), path(d, d.name.toString))

        /** val */
        case v: ValDef if !v.symbol.is(Flags.ModuleVal) =>
          Val(v.name.toString, comment, flags(v), path(v, v.name.toString))

        case x => {
          dottydoc.println(s"Found unwanted entity: $x (${x.pos}, ${comment})\n${x.show}")
          NonEntity
        }
      }
    }

    var packages: Map[String, Package] = Map.empty

    def addEntity(p: Package): Unit = {
      val path = p.path.mkString(".")
      packages = packages + (path -> packages.get(path).map { ex =>
        val children = (ex.children ::: p.children).distinct.sortBy(_.name)
        val comment = ex.comment.orElse(p.comment)
        Package(p.name, children, comment, p.path)
      }.getOrElse(p))
    }

    override def run(implicit ctx: Context): Unit =
      collect(ctx.compilationUnit.tpdTree) match {
        case p: Package => addEntity(p)
        case _ => ()
      }

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
      val compUnits = super.runOn(units)
      util.IndexWriters.writeJs(packages, "../js/out")
      compUnits
    }
  }

}
