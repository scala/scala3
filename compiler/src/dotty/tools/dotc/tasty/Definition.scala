package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Contexts.Context

object Definition {

  def apply(tree: Tree)(implicit ctx: Context): scala.tasty.Definition = Impl(tree, ctx)

  object ValDef {
    def unapply(term: scala.tasty.Definition): Option[(scala.tasty.TermName, scala.tasty.TypeTree, Option[scala.tasty.Term], List[scala.tasty.Modifier])] = term match {
      case Impl(vdef @ Trees.ValDef(name, tpt, _), ctx) =>
        implicit val ctx_ = ctx
        Some((TermName(name), TypeTree(tpt), if (vdef.rhs.isEmpty) None else Some(Term(vdef.rhs)), vdef.rawMods.mods.map(mod => Modifier(mod))))
      case _ => None
    }
  }
  
//  case class DefDef(name: TermName, typeParams: List[TypeDef], paramss: List[List[ValDef]],
//                    returnTpt: Term, rhs: Option[Term], mods: List[Modifier]) extends Definition

//  case class TypeDef(name: TypeName, rhs: Term, mods: List[Modifier]) extends Definition

//  case class ClassDef(name: TypeName, constructor: DefDef, parents: List[Term],
//                      self: Option[ValDef], body: List[Statement], mods: List[Modifier]) extends Definition

  private case class Impl(tree: Tree, ctx: Context) extends scala.tasty.Definition with Positioned {

    def tpe: scala.tasty.Type = Type(tree.tpe)(ctx)

    override def name: scala.tasty.Name = ???

    override def owner: scala.tasty.Definition = ???

    override def toString: String = this match {
      case ValDef(name, tpt, rhs, mods) => s"ValDef($name, $tpt, $rhs, $mods)"
      case _ => s"Definition{## $tree ##}"
    }

    private def list(xs: List[_]): String =
      if (xs.isEmpty) "Nil" else xs.mkString("List(", ", ", ")")
  }

}
