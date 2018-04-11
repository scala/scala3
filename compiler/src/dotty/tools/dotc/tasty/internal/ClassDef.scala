package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statement

object ClassDef {

  def apply(tree: tpd.TypeDef)(implicit ctx: Context): statement.ClassDef = Impl(tree, ctx)

  def unapply(term: statement.TopLevelStatement): Option[statement.ClassDef.Data] = term match {
    case Impl(cdef @ Trees.TypeDef(name, impl@Trees.Template(constr, parents, self, _)), ctx) =>
      implicit val ctx_ = ctx
      if (cdef.symbol.isClass) Some((TypeName(name), DefDef(constr), parents.map(Term(_)), if (self.isEmpty) None else Some(ValDef(self)), impl.body.map(Statement(_)), cdef.rawMods.mods.map(Modifier(_))))
      else None
    case _ => None
  }

  private case class Impl(tree: tpd.TypeDef, ctx: Context) extends statement.ClassDef with Positioned {

    def tpe: scala.tasty.Type = Type(tree.tpe)(ctx)

    override def name: scala.tasty.Name = ???

    override def owner: statement.Definition = ???

    override def toString: String = this match {
      case ClassDef(name, constructor, parents, self, body, mods) =>
        s"ClassDef($name, $constructor, $parents, $self, $body, $mods)"
      case _ => s"ClassDef{## $tree ##}"
    }
  }

}
