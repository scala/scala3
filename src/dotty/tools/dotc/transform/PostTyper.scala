package dotty.tools.dotc
package transform

import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransform, TreeTransformer}
import dotty.tools.dotc.ast.{Trees, tpd}
import scala.collection.{ mutable, immutable }
import ValueClasses._
import scala.annotation.tailrec
import core._
import typer.ErrorReporting._
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
 *  (3) Add synthetic methods (@see SyntheticMethods)
 *      
 *  (4) Check that `New` nodes can be instantiated, and that annotations are valid
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
  val synthMth = new SyntheticMethods(thisTransformer)
  
  /** Check that `tp` refers to a nonAbstract class
   *  and that the instance conforms to the self type of the created class.
   */
  private def checkInstantiable(tp: Type, pos: Position)(implicit ctx: Context): Unit =
    tp.underlyingClassRef(refinementOK = false) match {
      case tref: TypeRef =>
        val cls = tref.symbol
        if (cls.is(AbstractOrTrait))
          ctx.error(d"$cls is abstract; cannot be instantiated", pos)
        if (!cls.is(Module)) {
          val selfType = tp.givenSelfType.asSeenFrom(tref.prefix, cls.owner)
          if (selfType.exists && !(tp <:< selfType))
            ctx.error(d"$tp does not conform to its self type $selfType; cannot be instantiated")
        }
      case _ =>
    }
  
  private def newPart(tree: Tree): Option[New] = methPart(tree) match {
    case Select(nu: New, _) => Some(nu)
    case _ => None
  }
  
  private def checkValidJavaAnnotation(annot: Tree)(implicit ctx: Context): Unit = {
    // TODO fill in
  }

  class PostTyperTransformer extends Transformer {
    
    private var inJavaAnnot: Boolean = false
    
    private var parentNews: Set[New] = Set()
    
    private def transformAnnot(annot: Tree)(implicit ctx: Context): Tree = {
      val saved = inJavaAnnot
      inJavaAnnot = annot.symbol is JavaDefined
      if (inJavaAnnot) checkValidJavaAnnotation(annot)
      try transform(annot)
      finally inJavaAnnot = saved
    }
    
    private def transformAnnot(annot: Annotation)(implicit ctx: Context): Annotation =
      annot.derivedAnnotation(transformAnnot(annot.tree))
    
    private def transformAnnots(tree: MemberDef)(implicit ctx: Context): Unit =
      tree.symbol.transformAnnotations(transformAnnot)

    override def transform(tree: Tree)(implicit ctx: Context): Tree =
      try tree match {
        case impl: Template =>
          val saved = parentNews
          parentNews ++= impl.parents.flatMap(newPart)
          try 
            synthMth.addSyntheticMethods(
              paramFwd.forwardParamAccessors(
                superAcc.wrapTemplate(impl)(
                  super.transform(_).asInstanceOf[Template])))
          finally parentNews = saved
        case tree @ TypeApply(sel: Select, args) =>
          val args1 = transform(args)
          val sel1 = superAcc.transformSelect(super.transform(sel), args1)
          if (superAcc.isProtectedAccessor(sel1)) sel1 else cpy.TypeApply(tree)(sel1, args1)
        case sel: Select =>
          superAcc.transformSelect(super.transform(sel), Nil)
        case tree @ Assign(sel: Select, _) =>
          superAcc.transformAssign(super.transform(tree))
        case tree: DefDef =>
          transformAnnots(tree)
          superAcc.wrapDefDef(tree)(super.transform(tree).asInstanceOf[DefDef])
        case tree: MemberDef =>
          transformAnnots(tree)
          super.transform(tree)
        case tree: New if !inJavaAnnot && !parentNews.contains(tree) =>
          checkInstantiable(tree.tpe, tree.pos)
          super.transform(tree)
        case Annotated(annot, annotated) =>
          cpy.Annotated(tree)(transformAnnot(annot), transform(annotated))
        case tree: TypeTree =>
          tree.withType(
            tree.tpe match {
              case AnnotatedType(annot, tpe) => AnnotatedType(transformAnnot(annot), tpe)
              case tpe => tpe
            }
          )
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
