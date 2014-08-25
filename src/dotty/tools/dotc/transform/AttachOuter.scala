package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import core.StdNames.nme
import ast.Trees._
import util.Attachment

/** This phase decorates News and parent constructors of non-static inner classes
 *  with an attachment indicating the outer reference as a tree. This is necessary because
 *  outer prefixes are erased, and explicit outer runs only after erasure.
 */
class AttachOuter extends MiniPhaseTransform {
  import ast.tpd._

  val Outer = new Attachment.Key[Tree]

  override def phaseName: String = "attachOuter"

  private def outerPrefix(tpe: Type)(implicit ctx: Context): Type = tpe match {
    case tpe: TypeRef =>
      tpe.symbol match {
        case cls: ClassSymbol =>
          if (cls.owner.isStaticOwner || cls.is(Interface)) NoPrefix
          else if (tpe.prefix eq NoPrefix) cls.owner.enclosingClass.thisType
          else tpe.prefix
        case _ =>
          outerPrefix(tpe.underlying)
      }
    case tpe: TypeProxy =>
      outerPrefix(tpe.underlying)
  }

  override def transformNew(tree: New)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val pre = outerPrefix(tree.tpt.tpe)
    pre match {
      case pre: SingletonType =>
        tree.putAttachment(Outer, singleton(pre)) match {
          case Some(outer) => assert(outer.tpe =:= pre)
          case none =>
        }
      case NoPrefix =>
    }
    tree
  }

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo): Tree = {
    def transformParent(tree: Tree): Tree = tree match {
      case tree: TypeTree if outerPrefix(tree.tpe) != NoPrefix =>
        val constr = New(tree.tpe, Nil).withPos(tree.pos)
        val Select(nu: New, _) = methPart(constr)
        transformNew(nu)
        constr
      case _ =>
        tree
    }
    cpy.Template(tree)(parents = tree.parents mapconserve transformParent)
  }
}