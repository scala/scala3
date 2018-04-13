package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty
import scala.tasty.statements
import scala.tasty.types

object ClassDef {

  def apply(tree: tpd.TypeDef)(implicit ctx: Context): statements.ClassDef = Impl(tree, ctx)

  def unapplyClassDef(term: statements.TopLevelStatement): Option[statements.ClassDef.Data] = term match {
    case Impl(cdef @ Trees.TypeDef(name, impl@Trees.Template(constr, parents, self, _)), ctx) =>
      implicit val ctx_ = ctx
      if (cdef.symbol.isClass) {
        val className = TypeName(name)
        val constructor = DefDef(constr)
        val classParents = parents.map(TermOrTypeTree(_))
        val selfVal = if (self.isEmpty) None else Some(ValDef(self))
        val body = impl.body.map(Statement(_))
        val mods = Modifiers(cdef)
        Some((className, constructor, classParents, selfVal, body, mods))
      }
      else None
    case _ => None
  }

  private case class Impl(tree: tpd.TypeDef, ctx: Context) extends statements.ClassDef with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    def sym: scala.tasty.Symbol = TastySymbol(tree.symbol(ctx))(ctx)

    override def toString: String = {
      import Toolbox.extractor
      val statements.ClassDef(name, constructor, parents, self, body, mods) = this
      s"ClassDef($name, $constructor, ${parents.map { case Left(p) => p; case Right(p) => p} }, $self, $body, $mods)"
    }
  }

}
