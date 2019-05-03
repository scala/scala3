package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import scala.collection.mutable
import core._
import dotty.tools.dotc.typer.Checking
import dotty.tools.dotc.typer.Inliner
import dotty.tools.dotc.typer.VarianceChecker
import Types._, Contexts._, Names._, Flags._, DenotTransformers._, Phases._
import SymDenotations._, StdNames._, Annotations._, Trees._, Scopes._
import Decorators._
import Symbols._, SymUtils._
import reporting.diagnostic.messages._

object PostTyper {
  val name: String = "posttyper"
}

/** A macro transform that runs immediately after typer and that performs the following functions:
 *
 *  (1) Add super accessors (@see SuperAccessors)
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
 *  (11) Minimizes `call` fields of `Inlined` nodes to just point to the toplevel
 *       class from which code was inlined.
 *
 *  The reason for making this a macro transform is that some functions (in particular
 *  super and protected accessors and instantiation checks) are naturally top-down and
 *  don't lend themselves to the bottom-up approach of a mini phase. The other two functions
 *  (forwarding param accessors and synthetic methods) only apply to templates and fit
 *  mini-phase or subfunction of a macro phase equally well. But taken by themselves
 *  they do not warrant their own group of miniphases before pickling.
 */
class PostTyper extends MacroTransform with IdentityDenotTransformer { thisPhase =>
  import tpd._

  /** the following two members override abstract members in Transform */
  override def phaseName: String = PostTyper.name

  override def checkPostCondition(tree: tpd.Tree)(implicit ctx: Context): Unit = tree match {
    case tree: ValOrDefDef =>
      assert(!tree.symbol.signature.isUnderDefined)
    case _ =>
  }

  override def changesMembers: Boolean = true // the phase adds super accessors and synthetic methods

  override def transformPhase(implicit ctx: Context): Phase = thisPhase.next

  protected def newTransformer(implicit ctx: Context): Transformer =
    new PostTyperTransformer

  val superAcc: SuperAccessors = new SuperAccessors(thisPhase)
  val paramFwd: ParamForwarding = new ParamForwarding(thisPhase)
  val synthMth: SyntheticMethods = new SyntheticMethods(thisPhase)

  private def newPart(tree: Tree): Option[New] = methPart(tree) match {
    case Select(nu: New, _) => Some(nu)
    case _ => None
  }

  private def checkValidJavaAnnotation(annot: Tree)(implicit ctx: Context): Unit = {
    // TODO fill in
  }

  class PostTyperTransformer extends Transformer {

    private[this] var inJavaAnnot: Boolean = false

    private[this] var noCheckNews: Set[New] = Set()

    def withNoCheckNews[T](ts: List[New])(op: => T): T = {
      val saved = noCheckNews
      noCheckNews ++= ts
      try op finally noCheckNews = saved
    }

    def isCheckable(t: New): Boolean = !inJavaAnnot && !noCheckNews.contains(t)

    private def transformAnnot(annot: Tree)(implicit ctx: Context): Tree = {
      val saved = inJavaAnnot
      inJavaAnnot = annot.symbol is JavaDefined
      if (inJavaAnnot) checkValidJavaAnnotation(annot)
      try transform(annot)
      finally inJavaAnnot = saved
    }

    private def transformAnnot(annot: Annotation)(implicit ctx: Context): Annotation =
      annot.derivedAnnotation(transformAnnot(annot.tree))

    private def processMemberDef(tree: Tree)(implicit ctx: Context): tree.type = {
      val sym = tree.symbol
      sym.transformAnnotations(transformAnnot)
      sym.defTree = tree
      tree
    }

    private def transformSelect(tree: Select, targs: List[Tree])(implicit ctx: Context): Tree = {
      val qual = tree.qualifier
      qual.symbol.moduleClass.denot match {
        case pkg: PackageClassDenotation =>
          val pobj = pkg.packageObjFor(tree.symbol)
          if (pobj.exists)
            return transformSelect(cpy.Select(tree)(qual.select(pobj), tree.name), targs)
        case _ =>
      }
      val tree1 = super.transform(tree)
      constToLiteral(tree1) match {
        case _: Literal => tree1
        case _ => superAcc.transformSelect(tree1, targs)
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
          case tp: PolyType if args.exists(isNamedArg) =>
            val (namedArgs, otherArgs) = args.partition(isNamedArg)
            val args1 = reorderArgs(tp.paramNames, namedArgs.asInstanceOf[List[NamedArg]], otherArgs)
            TypeApply(tycon, args1).withSpan(tree.span).withType(tree.tpe)
          case _ =>
            tree
        }
    }

    private object dropInlines extends TreeMap {
      override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case Inlined(call, _, _) =>
          cpy.Inlined(tree)(call, Nil, Typed(ref(defn.Predef_undefined), TypeTree(tree.tpe)).withSpan(tree.span))
        case _ => super.transform(tree)
      }
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree =
      try tree match {
        case tree: Ident if !tree.isType =>
          tree.tpe match {
            case tpe: ThisType => This(tpe.cls).withSpan(tree.span)
            case _ => tree
          }
        case tree @ Select(qual, name) =>
          if (name.isTypeName) {
            Checking.checkRealizable(qual.tpe, qual.posd)
            super.transform(tree)(ctx.addMode(Mode.Type))
          }
          else
            transformSelect(tree, Nil)
        case tree: Apply =>
          val methType = tree.fun.tpe.widen
          val app =
            if (methType.isErasedMethod)
              tpd.cpy.Apply(tree)(
                tree.fun,
                tree.args.mapConserve(arg =>
                  if (methType.isImplicitMethod && arg.span.isSynthetic) ref(defn.Predef_undefined)
                  else dropInlines.transform(arg)))
            else
              tree
          methPart(app) match {
            case Select(nu: New, nme.CONSTRUCTOR) if isCheckable(nu) =>
              // need to check instantiability here, because the type of the New itself
              // might be a type constructor.
              Checking.checkInstantiable(tree.tpe, nu.posd)
              withNoCheckNews(nu :: Nil)(super.transform(app))
            case _ =>
              super.transform(app)
          }
        case tree: TypeApply =>
          val tree1 @ TypeApply(fn, args) = normalizeTypeArgs(tree)
          if (fn.symbol != defn.ChildAnnot.primaryConstructor) {
            // Make an exception for ChildAnnot, which should really have AnyKind bounds
            Checking.checkBounds(args, fn.tpe.widen.asInstanceOf[PolyType])
          }
          fn match {
            case sel: Select =>
              val args1 = transform(args)
              val sel1 = transformSelect(sel, args1)
              cpy.TypeApply(tree1)(sel1, args1)
            case _ =>
              super.transform(tree1)
          }
        case Inlined(call, bindings, expansion) if !call.isEmpty =>
          val pos = call.sourcePos
          val callTrace = Inliner.inlineCallTrace(call.symbol, pos)(ctx.withSource(pos.source))
          cpy.Inlined(tree)(callTrace, transformSub(bindings), transform(expansion)(inlineContext(call)))
        case tree: Template =>
          withNoCheckNews(tree.parents.flatMap(newPart)) {
            val templ1 = paramFwd.forwardParamAccessors(tree)
            synthMth.addSyntheticMethods(
                superAcc.wrapTemplate(templ1)(
                  super.transform(_).asInstanceOf[Template]))
          }
        case tree: ValDef =>
          val tree1 = cpy.ValDef(tree)(rhs = normalizeErasedRhs(tree.rhs, tree.symbol))
          processMemberDef(super.transform(tree1))
        case tree: DefDef =>
          val tree1 = cpy.DefDef(tree)(rhs = normalizeErasedRhs(tree.rhs, tree.symbol))
          processMemberDef(superAcc.wrapDefDef(tree1)(super.transform(tree1).asInstanceOf[DefDef]))
        case tree: TypeDef =>
          val sym = tree.symbol
          if (sym.isClass) {
            // Add SourceFile annotation to top-level classes
            if (sym.owner.is(Package) &&
              ctx.compilationUnit.source.exists &&
              sym != defn.SourceFileAnnot)
              sym.addAnnotation(Annotation.makeSourceFile(ctx.compilationUnit.source.file.path))
          }
          processMemberDef(super.transform(tree))
        case tree: New if isCheckable(tree) =>
          Checking.checkInstantiable(tree.tpe, tree.posd)
          super.transform(tree)
        case tree: Closure if !tree.tpt.isEmpty =>
          Checking.checkRealizable(tree.tpt.tpe, tree.posd, "SAM type")
          super.transform(tree)
        case tree @ Annotated(annotated, annot) =>
          cpy.Annotated(tree)(transform(annotated), transformAnnot(annot))
        case tree: AppliedTypeTree =>
          if (tree.tpt.symbol == defn.andType)
            Checking.checkNonCyclicInherited(tree.tpe, tree.args.tpes, EmptyScope, tree.posd)
              // Ideally, this should be done by Typer, but we run into cyclic references
              // when trying to typecheck self types which are intersections.
          else if (tree.tpt.symbol == defn.orType)
            () // nothing to do
          else
            Checking.checkAppliedType(tree, boundsCheck = !ctx.mode.is(Mode.Pattern))
          super.transform(tree)
        case SingletonTypeTree(ref) =>
          Checking.checkRealizable(ref.tpe, ref.posd)
          super.transform(tree)
        case tree: TypeTree =>
          tree.withType(
            tree.tpe match {
              case AnnotatedType(tpe, annot) => AnnotatedType(tpe, transformAnnot(annot))
              case tpe => tpe
            }
          )
        case tree: LambdaTypeTree =>
          VarianceChecker.checkLambda(tree)
          super.transform(tree)
        case Import(_, expr, selectors) =>
          val exprTpe = expr.tpe
          val seen = mutable.Set.empty[Name]
          def checkIdent(ident: untpd.Ident): Unit = {
            val name = ident.name.asTermName
            if (name != nme.WILDCARD && !exprTpe.member(name).exists && !exprTpe.member(name.toTypeName).exists)
              ctx.error(NotAMember(exprTpe, name, "value"), ident.sourcePos)
            if (seen(ident.name))
              ctx.error(ImportRenamedTwice(ident), ident.sourcePos)
            seen += ident.name
          }
          selectors.foreach {
            case ident: untpd.Ident                 => checkIdent(ident)
            case Thicket((ident: untpd.Ident) :: _) => checkIdent(ident)
            case _                                  =>
          }
          super.transform(tree)
        case Typed(Ident(nme.WILDCARD), _) =>
          super.transform(tree)(ctx.addMode(Mode.Pattern))
            // The added mode signals that bounds in a pattern need not
            // conform to selector bounds. I.e. assume
            //     type Tree[T >: Null <: Type]
            // One is still allowed to write
            //     case x: Tree[_]
            // (which translates to)
            //     case x: (_: Tree[_])
        case tree =>
          super.transform(tree)
      }
      catch {
        case ex : AssertionError =>
          println(i"error while transforming $tree")
          throw ex
      }

    /** Transforms the rhs tree into a its default tree if it is in an `erased` val/def.
    *  Performed to shrink the tree that is known to be erased later.
    */
    private def normalizeErasedRhs(rhs: Tree, sym: Symbol)(implicit ctx: Context) =
      if (sym.isEffectivelyErased) dropInlines.transform(rhs) else rhs
  }
}
