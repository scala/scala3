package dotty.tools.dotc
package parsing

import ast.NavigateAST.precedingTrees
import core.Comments._
import core.Flags
import core.Decorators._
import core.Contexts._
import ast.{Positioned, Trees, untpd}
import util.SourceFile

object CheckAlignments {
  import untpd._

  private def kindString(t: Positioned)(implicit ctx: Context) = t match {
    case t: ValDef              => if (t.mods.is(Flags.Mutable)) "var" else "val"
    case _: DefDef              => "def"
    case t: TypeDef             => if (t.isClassDef) "class" else "type"
    case _: ModuleDef           => "object"
    case _: PackageDef          => "package"
    case _: If                  => "if"
    case _: Try                 => "try"
    case _: Match               => "match"
    case _: WhileDo             => "while"
    case _: DoWhile             => "do"
    case _: ForDo | _: ForYield => "for"
    case _                      => ""
  }

  private def definedString(t: Positioned) = t match {
    case t: MemberDef => t.name.toString
    case _            => ""
  }

  def checkEndComments(source: SourceFile, endComments: List[Comment], roots: List[Tree])(implicit ctx: Context) = {
    for (ec <- endComments) {
      val endStr = ec.endCommentString
      val column = source.column(ec.pos.start)
      def misAligned(other: String): String =
        i"misaligned '// end', corresponds to $other"
      var warning: String = misAligned("nothing")
      def combine(s1: String, s2: String) =
        if (s1.isEmpty)
          if (s2.isEmpty) "nothing" else s"'$s2'"
        else
          if (s2.isEmpty) s"'$s1'" else s"'$s1 $s2'"
      def checkMatch(ts: List[Positioned]): Unit = ts match {
        case t :: ts1 =>
          if (source.column(t.pos.start) == column) {
            if (endStr == kindString(t) || endStr == definedString(t))
              warning = ""
            else {
              if (kindString(t).nonEmpty)
                warning = misAligned(combine(kindString(t), definedString(t)))
              checkMatch(ts1)
            }
          } else checkMatch(ts1)
        case Nil =>
      }
      checkMatch(precedingTrees(ec.pos.start, roots))
      if (warning.nonEmpty) ctx.warning(warning, ec.pos)
    }
  }
}