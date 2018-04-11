package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

object ClassDef {

  def apply(tree: tpd.TypeDef)(implicit ctx: Context): scala.tasty.ClassDef = Impl(tree, ctx)

  object ClassDef {
    def unapply(term: scala.tasty.Definition): Option[(scala.tasty.TypeName, scala.tasty.DefDef, List[scala.tasty.Term],  Option[scala.tasty.ValDef], List[scala.tasty.Statement], List[scala.tasty.Modifier])] = term match {
      case Impl(cdef @ Trees.TypeDef(name, impl@Trees.Template(constr, parents, self, _)), ctx) =>
        implicit val ctx_ = ctx
        if (cdef.symbol.isClass) Some((TypeName(name), DefDef(constr), parents.map(Term(_)), if (self.isEmpty) None else Some(ValDef(self)), impl.body.map(Statement(_)), cdef.rawMods.mods.map(Modifier(_))))
        else None
      case _ => None
    }
  }

  private case class Impl(tree: tpd.TypeDef, ctx: Context) extends scala.tasty.ClassDef with Positioned {

    def tpe: scala.tasty.Type = Type(tree.tpe)(ctx)

    override def name: scala.tasty.Name = ???

    override def owner: scala.tasty.Definition = ???

    override def toString: String = this match {
      case ClassDef(name, constructor, parents, self, body, mods) =>
        s"ClassDef($name, $constructor, $parents, $self, $body, $mods)"
      case _ => s"ClassDef{## $tree ##}"
    }
  }

}
