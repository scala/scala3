package dotty.tools.dotc
package transform

import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransform, TreeTransformer}
import dotty.tools.dotc.ast.{Trees, tpd, untpd}
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
import config.Printers.typr
import Symbols._, TypeUtils._
import reporting.diagnostic.messages.SuperCallsNotAllowedInline

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
 *  (10) Adds Child annotations to all sealed classes
 *
 *  (11) Minimizes `call` fields of `Inline` nodes to just point to the toplevel
 *       class from which code was inlined.
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

  override def changesMembers = true // the phase adds super accessors and synthetic methods

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

    private var noCheckNews: Set[New] = Set()

    def withNoCheckNews[T](ts: List[New])(op: => T): T = {
      val saved = noCheckNews
      noCheckNews ++= ts
      try op finally noCheckNews = saved
    }

    def isCheckable(t: New) = !inJavaAnnot && !noCheckNews.contains(t)

    private def transformAnnot(annot: Tree)(implicit ctx: Context): Tree = {
      val saved = inJavaAnnot
      inJavaAnnot = annot.symbol is JavaDefined
      if (inJavaAnnot) checkValidJavaAnnotation(annot)
      try transform(annot)
      finally inJavaAnnot = saved
    }

    private def transformAnnot(annot: Annotation)(implicit ctx: Context): Annotation =
      annot.derivedAnnotation(transformAnnot(annot.tree))

    private def registerChild(sym: Symbol, tp: Type)(implicit ctx: Context) = {
      val cls = tp.classSymbol
      if (cls.is(Sealed)) cls.addAnnotation(Annotation.makeChild(sym))
    }

    private def transformMemberDef(tree: MemberDef)(implicit ctx: Context): Unit = {
      val sym = tree.symbol
      if (sym.is(CaseVal, butNot = Method | Module)) registerChild(sym, sym.info)
      sym.transformAnnotations(transformAnnot)
    }

    private def transformSelect(tree: Select, targs: List[Tree])(implicit ctx: Context): Tree = {
      val qual = tree.qualifier
      qual.symbol.moduleClass.denot match {
        case pkg: PackageClassDenotation if !tree.symbol.maybeOwner.is(Package) =>
          transformSelect(cpy.Select(tree)(qual select pkg.packageObj.symbol, tree.name), targs)
        case _ =>
          val tree1 = super.transform(tree)
          constToLiteral(tree1) match {
            case _: Literal => tree1
            case _ => superAcc.transformSelect(tree1, targs)
          }
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
      try tree match {
        case tree: Ident if !tree.isType =>
          tree.tpe match {
            case tpe: ThisType => This(tpe.cls).withPos(tree.pos)
            case _ => paramFwd.adaptRef(fixSignature(tree))
          }
        case tree @ Select(qual, name) =>
          if (name.isTypeName) {
            Checking.checkRealizable(qual.tpe, qual.pos.focus)
            super.transform(tree)
          }
          else
            transformSelect(paramFwd.adaptRef(fixSignature(tree)), Nil)
        case tree: Super =>
          if (ctx.owner.enclosingMethod.isInlineMethod)
            ctx.error(SuperCallsNotAllowedInline(ctx.owner), tree.pos)
          super.transform(tree)
        case tree: Apply =>
          methPart(tree) match {
            case Select(nu: New, nme.CONSTRUCTOR) if isCheckable(nu) =>
              // need to check instantiability here, because the type of the New itself
              // might be a type constructor.
              Checking.checkInstantiable(tree.tpe, nu.pos)
              withNoCheckNews(nu :: Nil)(super.transform(tree))
            case _ =>
              super.transform(tree)
          }
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
        case Inlined(call, bindings, expansion) =>
          // Leave only a call trace consisting of
          //  - a reference to the top-level class from which the call was inlined,
          //  - the call's position
          // in the call field of an Inlined node.
          // The trace has enough info to completely reconstruct positions.
          // The minimization is done for two reasons:
          //  1. To save space (calls might contain large inline arguments, which would otherwise
          //     be duplicated
          //  2. To enable correct pickling (calls can share symbols with the inlined code, which
          //     would trigger an assertion when pickling).
          val callTrace = Ident(call.symbol.topLevelClass.typeRef).withPos(call.pos)
          cpy.Inlined(tree)(callTrace, transformSub(bindings), transform(expansion))
        case tree: Template =>
          withNoCheckNews(tree.parents.flatMap(newPart)) {
            val templ1 = paramFwd.forwardParamAccessors(tree)
            synthMth.addSyntheticMethods(
                superAcc.wrapTemplate(templ1)(
                  super.transform(_).asInstanceOf[Template]))
          }
        case tree: DefDef =>
          transformMemberDef(tree)
          superAcc.wrapDefDef(tree)(super.transform(tree).asInstanceOf[DefDef])
        case tree: TypeDef =>
          transformMemberDef(tree)
          val sym = tree.symbol
          if (sym.isClass) {
            // Add SourceFile annotation to top-level classes
            if (sym.owner.is(Package) &&
              ctx.compilationUnit.source.exists &&
              sym != defn.SourceFileAnnot)
              sym.addAnnotation(Annotation.makeSourceFile(ctx.compilationUnit.source.file.path))

            // Add Child annotation to sealed parents unless current class is anonymous
            if (!sym.isAnonymousClass) // ignore anonymous class
              sym.asClass.classParents.foreach { parent =>
                val sym2 = if (sym.is(Module)) sym.sourceModule else sym
                registerChild(sym2, parent)
              }

            tree
          }
          super.transform(tree)
        case tree: MemberDef =>
          transformMemberDef(tree)
          super.transform(tree)
        case tree: New if isCheckable(tree) =>
          Checking.checkInstantiable(tree.tpe, tree.pos)
          super.transform(tree)
        case tree @ Annotated(annotated, annot) =>
          cpy.Annotated(tree)(transform(annotated), transformAnnot(annot))
        case tree: AppliedTypeTree =>
          Checking.checkAppliedType(tree)
          super.transform(tree)
        case SingletonTypeTree(ref) =>
          Checking.checkRealizable(ref.tpe, ref.pos.focus)
          super.transform(tree)
        case tree: TypeTree =>
          tree.withType(
            tree.tpe match {
              case AnnotatedType(tpe, annot) => AnnotatedType(tpe, transformAnnot(annot))
              case tpe => tpe
            }
          )
        case Import(expr, selectors) =>
          val exprTpe = expr.tpe
          def checkIdent(ident: untpd.Ident): Unit = {
            val name = ident.name.asTermName
            if (name != nme.WILDCARD && !exprTpe.member(name).exists && !exprTpe.member(name.toTypeName).exists)
              ctx.error(s"${ident.name} is not a member of ${expr.show}", ident.pos)
          }
          selectors.foreach {
            case ident: untpd.Ident                 => checkIdent(ident)
            case Thicket((ident: untpd.Ident) :: _) => checkIdent(ident)
            case _                                  =>
          }
          super.transform(tree)
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
