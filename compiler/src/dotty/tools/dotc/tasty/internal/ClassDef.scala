package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statements
import scala.tasty.types

object ClassDef {

  def apply(tree: tpd.TypeDef)(implicit ctx: Context): statements.ClassDef = Impl(tree, ctx)

  def unapplyClassDef(term: statements.TopLevelStatement): Option[statements.ClassDef.Data] = term match {
    case Impl(cdef @ Trees.TypeDef(name, impl@Trees.Template(constr, parents, self, _)), ctx) =>
      implicit val ctx_ = ctx
      if (cdef.symbol.isClass) Some((TypeName(name), DefDef(constr), parents.map(Term(_)), if (self.isEmpty) None else Some(ValDef(self)), impl.body.map(Statement(_)), cdef.rawMods.mods.map(Modifier(_))))
      else None
    case _ => None
  }

  private case class Impl(tree: tpd.TypeDef, ctx: Context) extends statements.ClassDef with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    override def name: scala.tasty.names.Name = ???

    override def owner: statements.Definition = ???

    override def toString: String = {
      import Toolbox.extractor
      val statements.ClassDef(name, constructor, parents, self, body, mods) = this
      s"ClassDef($name, $constructor, $parents, $self, $body, $mods)"
    }
  }

}
