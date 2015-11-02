package dotty.tools.dotc
package transform

import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransform, TreeTransformer}
import dotty.tools.dotc.ast.{Trees, tpd}
import scala.collection.{ mutable, immutable }
import ValueClasses._
import scala.annotation.tailrec
import core._
import typer.ErrorReporting._
import typer.Checking
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
 *  (5) Convert all trees representing types to TypeTrees.
 *
 *  (6) Check the bounds of AppliedTypeTrees
 *
 *  (7) Insert `.package` for selections of package object members
 *
 *  (8) Replaces self references by name with `this`
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

  private def newPart(tree: Tree): Option[New] = methPart(tree) match {
    case Select(nu: New, _) => Some(nu)
    case _ => None
  }

  private def checkValidJavaAnnotation(annot: Tree)(implicit ctx: Context): Unit = {
    // TODO fill in
  }

  /** Check bounds of AppliedTypeTrees.
   *  Replace type trees with TypeTree nodes.
   *  Replace constant expressions with Literal nodes.
   *  Note: Demanding idempotency instead of purity in literalize is strictly speaking too loose.
   *  Example
   *
   *    object O { final val x = 42; println("43") }
   *    O.x
   *
   *  Strictly speaking we can't replace `O.x` with `42`.  But this would make
   *  most expressions non-constant. Maybe we can change the spec to accept this
   *  kind of eliding behavior. Or else enforce true purity in the compiler.
   *  The choice will be affected by what we will do with `inline` and with
   *  Singleton type bounds (see SIP 23). Presumably
   *
   *     object O1 { val x: Singleton = 42; println("43") }
   *     object O2 { inline val x = 42; println("43") }
   *
   *  should behave differently.
   *
   *     O1.x  should have the same effect as   { println("43"; 42 }
   *
   *  whereas
   *
   *     O2.x = 42
   *
   *  Revisit this issue once we have implemented `inline`. Then we can demand
   *  purity of the prefix unless the selection goes to an inline val.
   */
  private def normalizeTree(tree: Tree)(implicit ctx: Context): Tree = {
    def literalize(tp: Type): Tree = tp.widenTermRefExpr match {
      case ConstantType(value) if isIdempotentExpr(tree) => Literal(value)
      case _ => tree
    }
    def norm(tree: Tree) =
      if (tree.isType) TypeTree(tree.tpe).withPos(tree.pos)
      else literalize(tree.tpe)
    tree match {
      case tree: TypeTree =>
        tree
      case AppliedTypeTree(tycon, args) =>
        val tparams = tycon.tpe.typeSymbol.typeParams
        val bounds = tparams.map(tparam =>
          tparam.info.asSeenFrom(tycon.tpe.normalizedPrefix, tparam.owner.owner).bounds)
        Checking.checkBounds(args, bounds, _.substDealias(tparams, _))
        norm(tree)
      case _ =>
        norm(tree)
    }
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

    private def transformSelect(tree: Select, targs: List[Tree])(implicit ctx: Context): Tree = {
      val qual = tree.qualifier
      qual.symbol.moduleClass.denot match {
        case pkg: PackageClassDenotation if !tree.symbol.maybeOwner.is(Package) =>
          transformSelect(cpy.Select(tree)(qual select pkg.packageObj.symbol, tree.name), targs)
        case _ =>
          superAcc.transformSelect(super.transform(tree), targs)
      }
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree =
      try normalizeTree(tree) match {
        case tree: Ident =>
          tree.tpe match {
            case tpe: ThisType => This(tpe.cls).withPos(tree.pos)
            case _ => tree
          }
        case tree: Select =>
          transformSelect(tree, Nil)
        case tree @ TypeApply(sel: Select, args) =>
          val args1 = transform(args)
          val sel1 = transformSelect(sel, args1)
          if (superAcc.isProtectedAccessor(sel1)) sel1 else cpy.TypeApply(tree)(sel1, args1)
        case tree @ Assign(sel: Select, _) =>
          superAcc.transformAssign(super.transform(tree))
        case tree: Template =>
          val saved = parentNews
          parentNews ++= tree.parents.flatMap(newPart)
          try
            synthMth.addSyntheticMethods(
              paramFwd.forwardParamAccessors(
                superAcc.wrapTemplate(tree)(
                  super.transform(_).asInstanceOf[Template])))
          finally parentNews = saved
        case tree: DefDef =>
          transformAnnots(tree)
          superAcc.wrapDefDef(tree)(super.transform(tree).asInstanceOf[DefDef])
        case tree: MemberDef =>
          transformAnnots(tree)
          super.transform(tree)
        case tree: New if !inJavaAnnot && !parentNews.contains(tree) =>
          Checking.checkInstantiable(tree.tpe, tree.pos)
          super.transform(tree)
        case tree @ Annotated(annot, annotated) =>
          cpy.Annotated(tree)(transformAnnot(annot), transform(annotated))
        case tree: TypeTree =>
          tree.withType(
            tree.tpe match {
              case AnnotatedType(annot, tpe) => AnnotatedType(transformAnnot(annot), tpe)
              case tpe => tpe
            }
          )
        case tree =>
          super.transform(tree)
      }
      catch {
        case ex : AssertionError =>
          println(i"error while transforming $tree")
          throw ex
      }
  }
}
