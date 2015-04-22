package dotty.tools.dotc
package transform

import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransform, TreeTransformer}
import dotty.tools.dotc.ast.{Trees, tpd}
import scala.collection.{ mutable, immutable }
import ValueClasses._
import scala.annotation.tailrec
import core._
import typer.InstChecks
import Types._, Contexts._, Constants._, Names._, NameOps._, Flags._, DenotTransformers._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Scopes._, Denotations._
import util.Positions._
import Decorators._
import Symbols._, TypeUtils._

/** A macro transform that runs immediately after typer and that performs the following functions:
 *  
 *  (1) Add super accessors and protected accessors (@see SuperAccessors)
 *  
 *  (2) Convert parameter fields that have the same name as a corresponding
 *      public parameter field in a superclass to a forwarder to the superclass
 *      field (corresponding = super class field is initialized with subclass field)
 *      (@see ForwardParamAccessors)
 *  
 *  The reason for making this a macro transform is that some functions (in particular
 *  super and protected accessors and instantiation checks) are naturally top-down and
 *  don't lend themselves to the bottom-up approach of a mini phase. The other two functions
 *  (forwarding param accessors and synthetic methods) only apply to templates and fit
 *  mini-phase or subfunction of a macro phase equally well. But taken by themselves
 *  they do not warrant their own group of miniphases before pickling.
 */
class PostTyper extends MacroTransform with IdentityDenotTransformer  { thisTransformer =>

  import tpd._

  /** the following two members override abstract members in Transform */
  override def phaseName: String = "posttyper"

  override def transformPhase(implicit ctx: Context) = thisTransformer.next

  protected def newTransformer(implicit ctx: Context): Transformer =
    new PostTyperTransformer
    
  val superAcc = new SuperAccessors(thisTransformer)
  val paramFwd = new ParamForwarding(thisTransformer)
//  val instChecks = new InstChecks

  class PostTyperTransformer extends Transformer {

    override def transform(tree: Tree)(implicit ctx: Context): Tree =
      try tree match {
        case impl: Template =>
          def trans(impl: Template) = 
            cpy.Template(impl)(body = transformStats(impl.body, impl.symbol))
          paramFwd.forwardParamAccessors(superAcc.wrapTemplate(impl)(trans))
        case tree @ TypeApply(sel: Select, args) =>
          val sel1 = superAcc.transformSelect(super.transform(sel), args)
          if (superAcc.isProtectedAccessor(sel1)) sel1 else cpy.TypeApply(tree)(sel1, args)
        case sel: Select =>
          superAcc.transformSelect(super.transform(sel), Nil)
        case tree: DefDef =>
          superAcc.wrapDefDef(tree)(cpy.DefDef(tree)(rhs = transform(tree.rhs)))
        case tree: Assign =>
          superAcc.transformAssign(super.transform(tree))
//        case tree: Apply if tree.symbol.isConstructor =>
//          instChecks.checkInstantiable(tree)
//          super.transform(tree)
        case _ =>
          super.transform(tree)
      }
      catch {
        case ex : AssertionError =>
          println(i"error while transforming $tree")
          throw ex
      }
  }
}
