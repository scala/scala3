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
import config.Printers._
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
 *  (9) Adds SourceFile annotations to all top-level classes and objects
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

  /** Check bounds of AppliedTypeTrees and TypeApplys.
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
  private def normalizeTree(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case tree: TypeTree => tree
    case TypeApply(fn, args) =>
      Checking.checkBounds(args, fn.tpe.widen.asInstanceOf[PolyType])
      tree
    case _ =>
      if (tree.isType) {
        Checking.typeChecker.traverse(tree)
        TypeTree(tree.tpe).withPos(tree.pos)
      }
      else tree.tpe.widenTermRefExpr match {
        case ConstantType(value) if isIdempotentExpr(tree) => Literal(value)
        case _ => tree
      }
  }

  /** If the type of `tree` is a TermRefWithSignature with an underdefined
   *  signature, narrow the type by re-computing the signature (which should
   *  be fully-defined by now).
   */
  private def fixSignature[T <: Tree](tree: T)(implicit ctx: Context): T = tree.tpe match {
    case tpe: TermRefWithSignature if tpe.signature.isUnderDefined =>
      typr.println(i"fixing $tree with type ${tree.tpe.widen.toString} with sig ${tpe.signature} to ${tpe.widen.signature}")
      tree.withType(TermRef.withSig(tpe.prefix, tpe.name, tpe.widen.signature)).asInstanceOf[T]
    case _ => tree
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

    private def transformMemberDef(tree: MemberDef)(implicit ctx: Context): Unit = {
      val sym = tree.symbol
      sym.transformAnnotations(transformAnnot)
      if (!sym.is(SyntheticOrPrivate) && sym.owner.isClass) {
        val info1 = Checking.checkNoPrivateLeaks(sym, tree.pos)
        if (info1 ne sym.info)
          sym.copySymDenotation(info = info1).installAfter(thisTransformer)
      }
    }

    private def transformSelect(tree: Select, targs: List[Tree])(implicit ctx: Context): Tree = {
      val qual = tree.qualifier
      qual.symbol.moduleClass.denot match {
        case pkg: PackageClassDenotation if !tree.symbol.maybeOwner.is(Package) =>
          transformSelect(cpy.Select(tree)(qual select pkg.packageObj.symbol, tree.name), targs)
        case _ =>
          superAcc.transformSelect(super.transform(tree), targs)
      }
    }

    private def normalizeTypeArgs(tree: TypeApply)(implicit ctx: Context): TypeApply = tree.tpe match {
      case pt: PolyType => // wait for more arguments coming
        tree
      case _ =>
        def decompose(tree: TypeApply): (Tree, List[Tree]) = tree.fun match {
          case fun: TypeApply =>
            val (tycon, args) = decompose(fun)
            (tycon, args ++ tree.args)
          case _ =>
            (tree.fun, tree.args)
        }
        def reorderArgs(pnames: List[Name], namedArgs: List[NamedArg], otherArgs: List[Tree]): List[Tree] = pnames match {
          case pname :: pnames1 =>
            namedArgs.partition(_.name == pname) match {
              case (NamedArg(_, arg) :: _, namedArgs1) =>
                arg :: reorderArgs(pnames1, namedArgs1, otherArgs)
              case _ =>
                val otherArg :: otherArgs1 = otherArgs
                otherArg :: reorderArgs(pnames1, namedArgs, otherArgs1)
            }
          case nil =>
            assert(namedArgs.isEmpty && otherArgs.isEmpty)
            Nil
        }
        val (tycon, args) = decompose(tree)
        tycon.tpe.widen match {
          case tp: PolyType =>
            val (namedArgs, otherArgs) = args.partition(isNamedArg)
            val args1 = reorderArgs(tp.paramNames, namedArgs.asInstanceOf[List[NamedArg]], otherArgs)
            TypeApply(tycon, args1).withPos(tree.pos).withType(tree.tpe)
          case _ =>
            tree
        }
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree =
      try normalizeTree(tree) match {
        case tree: Ident =>
          tree.tpe match {
            case tpe: ThisType => This(tpe.cls).withPos(tree.pos)
            case _ => paramFwd.adaptRef(fixSignature(tree))
          }
        case tree: Select =>
          transformSelect(paramFwd.adaptRef(fixSignature(tree)), Nil)
        case tree: TypeApply =>
          val tree1 @ TypeApply(fn, args) = normalizeTypeArgs(tree)
          Checking.checkBounds(args, fn.tpe.widen.asInstanceOf[PolyType])
          fn match {
            case sel: Select =>
              val args1 = transform(args)
              val sel1 = transformSelect(sel, args1)
              if (superAcc.isProtectedAccessor(sel1)) sel1 else cpy.TypeApply(tree1)(sel1, args1)
            case _ =>
              super.transform(tree1)
          }
        case tree @ Assign(sel: Select, _) =>
          superAcc.transformAssign(super.transform(tree))
        case tree: Template =>
          val saved = parentNews
          parentNews ++= tree.parents.flatMap(newPart)
          try {
            val templ1 = paramFwd.forwardParamAccessors(tree)
            synthMth.addSyntheticMethods(
                superAcc.wrapTemplate(templ1)(
                  super.transform(_).asInstanceOf[Template]))
          }
          finally parentNews = saved
        case tree: DefDef =>
          transformMemberDef(tree)
          superAcc.wrapDefDef(tree)(super.transform(tree).asInstanceOf[DefDef])
        case tree: TypeDef =>
          transformMemberDef(tree)
          val sym = tree.symbol
          val tree1 =
            if (sym.isClass) {
              if (sym.owner.is(Package) &&
                  ctx.compilationUnit.source.exists &&
                  sym != defn.SourceFileAnnot)
                sym.addAnnotation(Annotation.makeSourceFile(ctx.compilationUnit.source.file.path))
              tree
            }
            else {
              Checking.typeChecker.traverse(tree.rhs)
              cpy.TypeDef(tree)(rhs = TypeTree(tree.symbol.info))
            }
          super.transform(tree1)
        case tree: MemberDef =>
          transformMemberDef(tree)
          super.transform(tree)
        case tree: New if !inJavaAnnot && !parentNews.contains(tree) =>
          Checking.checkInstantiable(tree.tpe, tree.pos)
          super.transform(tree)
        case tree @ Annotated(annot, annotated) =>
          cpy.Annotated(tree)(transformAnnot(annot), transform(annotated))
        case tree: TypeTree =>
          tree.withType(
            tree.tpe match {
              case AnnotatedType(tpe, annot) => AnnotatedType(tpe, transformAnnot(annot))
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
