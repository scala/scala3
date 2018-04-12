package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statements
import scala.tasty.types

object ValDef {

  def apply(tree: tpd.ValDef)(implicit ctx: Context): statements.ValDef = Impl(tree, ctx)

  def unapplyValDef(arg: statements.TopLevelStatement): Option[statements.ValDef.Data] = arg match {
    case Impl(vdef, ctx) =>
      implicit val ctx_ = ctx
      Some((TermName(vdef.name), TypeTree(vdef.tpt), if (vdef.rhs.isEmpty) None else Some(Term(vdef.rhs)), vdef.rawMods.mods.map(mod => Modifier(mod))))
    case _ => None
  }

  private case class Impl(tree: tpd.ValDef, ctx: Context) extends statements.ValDef with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    override def name: scala.tasty.names.Name = ???

    override def owner: statements.Definition = ???

    override def toString: String = {
      import Toolbox.extractor
      val statements.ValDef(name, tpt, rhs, mods) = this
      s"ValDef($name, $tpt, $rhs, $mods)"
    }

  }

}
