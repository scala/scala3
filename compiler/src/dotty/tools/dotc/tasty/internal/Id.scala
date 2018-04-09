package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.sourcePos

object Id {

  def apply(tree: untpd.Ident)(implicit ctx: Context): scala.tasty.Id =
    Impl(tree, new Position(tree.pos))

  private case class Impl(tree: untpd.Ident, pos: scala.tasty.Position) extends scala.tasty.Id with Positioned {

    def name: String = tree.name.toString

    override def toString: String = s"Id($name)"
  }
}
