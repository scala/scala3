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
import core.Names._
import core.NameOps._
import ast.Trees._
import SymUtils._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Phases.Phase
import util.Attachment
import collection.mutable

/** This phase adds outer accessors to classes and traits that need them.
 *  Compared to Scala 2.x, it tries to minimize the set of classes
 *  that take outer accessors by scanning class implementations for
 *  outer references.
 *
 *  The following things are delayed until erasure and are performed
 *  by class OuterOps:
 *
 *   - add outer parameters to constructors
 *   - pass outer arguments in constructor calls
 *
 *   replacement of outer this by outer paths is done in Erasure.
 *   needs to run after pattern matcher as it can add outer checks and force creation of $outer
 */
class ExplicitOuter extends MiniPhaseTransform with InfoTransformer { thisTransformer =>
  import ExplicitOuter._
  import ast.tpd._

  val Outer = new Attachment.Key[Tree]

  override def phaseName: String = "explicitOuter"

  /** List of names of phases that should have finished their processing of all compilation units
    * before this phase starts
    */
  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[PatternMatcher])

  /** Add outer accessors if a class always needs an outer pointer */
  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context) = tp match {
    case tp @ ClassInfo(_, cls, _, decls, _) if needsOuterAlways(cls) =>
      val newDecls = decls.cloneScope
      newOuterAccessors(cls).foreach(newDecls.enter)
      tp.derivedClassInfo(decls = newDecls)
    case _ =>
      tp
  }

  override def mayChange(sym: Symbol)(implicit ctx: Context): Boolean = sym.isClass

  /** First, add outer accessors if a class does not have them yet and it references an outer this.
   *  If the class has outer accessors, implement them.
   *  Furthermore, if a parent trait might have an outer accessor,
   *  provide an implementation for the outer accessor by computing the parent's
   *  outer from the parent type prefix. If the trait ends up not having an outer accessor
   *  after all, the implementation is redundant, but does not harm.
   *  The same logic is not done for non-trait parent classes because for them the outer
   *  pointer is passed in the super constructor, which will be implemented later in
   *  a separate phase which needs to run after erasure. However, we make sure here
   *  that the super class constructor is indeed a New, and not just a type.
   */
  override def transformTemplate(impl: Template)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val cls = ctx.owner.asClass
    val isTrait = cls.is(Trait)
    if (needsOuterIfReferenced(cls) &&
        !needsOuterAlways(cls) &&
        impl.existsSubTree(referencesOuter(cls, _)))
      ensureOuterAccessors(cls)
    if (hasOuter(cls)) {
      val newDefs = new mutable.ListBuffer[Tree]
      if (isTrait)
        newDefs += DefDef(outerAccessor(cls).asTerm, EmptyTree)
      else {
        val outerParamAcc = outerParamAccessor(cls)
        newDefs += ValDef(outerParamAcc, EmptyTree)
        newDefs += DefDef(outerAccessor(cls).asTerm, ref(outerParamAcc))
      }

      for (parentTrait <- cls.mixins) {
        if (needsOuterIfReferenced(parentTrait)) {
          val parentTp = cls.denot.thisType.baseTypeRef(parentTrait)
          val outerAccImpl = newOuterAccessor(cls, parentTrait).enteredAfter(thisTransformer)
          newDefs += DefDef(outerAccImpl, singleton(outerPrefix(parentTp)))
        }
      }

      val parents1 =
        for (parent <- impl.parents) yield {
          val parentCls = parent.tpe.classSymbol.asClass
          if (parentCls.is(Trait)) {
            parent
          }
          else parent match { // ensure class parent is a constructor
            case parent: TypeTree => New(parent.tpe, Nil).withPos(impl.pos)
            case _ => parent
          }
        }
      cpy.Template(impl)(parents = parents1, body = impl.body ++ newDefs)
    }
    else impl
  }

  override def transformClosure(tree: Closure)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    if (tree.tpt ne EmptyTree) {
      val cls = tree.tpt.asInstanceOf[TypeTree].tpe.classSymbol
      if (cls.exists && hasOuter(cls.asClass))
        ctx.error("Not a single abstract method type, requires an outer pointer", tree.pos)
    }
    tree
  }
}

object ExplicitOuter {
  import ast.tpd._

  /** Ensure that class `cls` has outer accessors */
  def ensureOuterAccessors(cls: ClassSymbol)(implicit ctx: Context): Unit = {
    //todo: implementing  #165 would simplify this logic
    val prevPhase = ctx.phase.prev
    assert(prevPhase.id <= ctx.explicitOuterPhase.id, "can add $outer symbols only before ExplicitOuter")
    assert(prevPhase.isInstanceOf[DenotTransformer], "adding outerAccessors requires being DenotTransformer")
    if (!hasOuter(cls)) {
      newOuterAccessors(cls).foreach(_.enteredAfter(prevPhase.asInstanceOf[DenotTransformer]))
    }
  }

  /** The outer accessor and potentially outer param accessor needed for class `cls` */
  private def newOuterAccessors(cls: ClassSymbol)(implicit ctx: Context) =
    newOuterAccessor(cls, cls) :: (if (cls is Trait) Nil else newOuterParamAccessor(cls) :: Nil)

  /** A new outer accessor or param accessor */
  private def newOuterSym(owner: ClassSymbol, cls: ClassSymbol, name: TermName, flags: FlagSet)(implicit ctx: Context) = {
    val target = cls.owner.enclosingClass.typeRef
    val info = if (flags.is(Method)) ExprType(target) else target
    ctx.newSymbol(owner, name, Synthetic | flags, info, coord = cls.coord)
  }

  /** A new param accessor for the outer field in class `cls` */
  private def newOuterParamAccessor(cls: ClassSymbol)(implicit ctx: Context) =
    newOuterSym(cls, cls, nme.OUTER, Private | ParamAccessor)

  /** A new outer accessor for class `cls` which is a member of `owner` */
  private def newOuterAccessor(owner: ClassSymbol, cls: ClassSymbol)(implicit ctx: Context) = {
    val deferredIfTrait = if (owner.is(Trait)) Deferred else EmptyFlags
    val outerAccIfOwn = if (owner == cls) OuterAccessor else EmptyFlags
    newOuterSym(owner, cls, outerAccName(cls),
      Final | Method | Stable | outerAccIfOwn | deferredIfTrait)
  }

  private def outerAccName(cls: ClassSymbol)(implicit ctx: Context): TermName =
    nme.OUTER.expandedName(cls)

  /** Class needs an outer pointer, provided there is a reference to an outer this in it. */
  def needsOuterIfReferenced(cls: ClassSymbol)(implicit ctx: Context): Boolean =
    !(cls.isStatic ||
      cls.owner.enclosingClass.isStaticOwner ||
      cls.is(PureInterface)
     )

  /** Class unconditionally needs an outer pointer. This is the case if
   *  the class needs an outer pointer if referenced and one of the following holds:
   *  - we might not know at all instantiation sites whether outer is referenced or not
   *  - we need to potentially pass along outer to a parent class or trait
   */
  private def needsOuterAlways(cls: ClassSymbol)(implicit ctx: Context): Boolean =
    needsOuterIfReferenced(cls) &&
    (!hasLocalInstantiation(cls) || // needs outer because we might not know whether outer is referenced or not
     cls.classInfo.parents.exists(parent => // needs outer to potentially pass along to parent
       needsOuterIfReferenced(parent.classSymbol.asClass)))

  /** Class is always instantiated in the compilation unit where it is defined */
  private def hasLocalInstantiation(cls: ClassSymbol)(implicit ctx: Context): Boolean =
    // scala2x modules always take an outer pointer(as of 2.11)
    // dotty modules are always locally instantiated
    cls.owner.isTerm || cls.is(Private) || cls.is(Module, butNot = Scala2x)

  /** The outer parameter accessor of cass `cls` */
  private def outerParamAccessor(cls: ClassSymbol)(implicit ctx: Context): TermSymbol =
    cls.info.decl(nme.OUTER).symbol.asTerm

  /** The outer accessor of class `cls`. To find it is a bit tricky. The
   *  class might have been moved with new owners between ExplicitOuter and Erasure,
   *  where the method is also called. For instance, it might have been part
   *  of a by-name argument, and therefore be moved under a closure method
   *  by ElimByName. In that case looking up the method again at Erasure with the
   *  fully qualified name `outerAccName` will fail, because the `outerAccName`'s
   *  result is phase dependent. In that case we use a backup strategy where we search all
   *  definitions in the class to find the one with the OuterAccessor flag.
   */
  def outerAccessor(cls: ClassSymbol)(implicit ctx: Context): Symbol =
    if (cls.isStatic) NoSymbol // fast return to avoid scanning package decls
    else cls.info.member(outerAccName(cls)).suchThat(_ is OuterAccessor).symbol orElse
      cls.info.decls.find(_ is OuterAccessor).getOrElse(NoSymbol)

  /** Class has an outer accessor. Can be called only after phase ExplicitOuter. */
  private def hasOuter(cls: ClassSymbol)(implicit ctx: Context): Boolean =
    needsOuterIfReferenced(cls) && outerAccessor(cls).exists

  /** Class constructor takes an outer argument. Can be called only after phase ExplicitOuter. */
  private def hasOuterParam(cls: ClassSymbol)(implicit ctx: Context): Boolean =
    !cls.is(Trait) && needsOuterIfReferenced(cls) && outerAccessor(cls).exists

  /** Tree references a an outer class of `cls` which is not a static owner.
   */
  def referencesOuter(cls: Symbol, tree: Tree)(implicit ctx: Context): Boolean = {
    def isOuter(sym: Symbol) =
      !sym.isStaticOwner && cls.isProperlyContainedIn(sym)
    tree match {
      case thisTree @ This(_) =>
        isOuter(thisTree.symbol)
      case id: Ident =>
        id.tpe match {
          case ref @ TermRef(NoPrefix, _) =>
            if (ref.symbol is Hoistable)
              // ref.symbol will be placed in enclosing class scope by LambdaLift, so it might need
              // an outer path then.
              isOuter(ref.symbol.owner.enclosingClass)
            else
              // ref.symbol will get a proxy in immediately enclosing class. If this properly
              // contains the current class, it needs an outer path.
              ctx.owner.enclosingClass.owner.enclosingClass.isContainedIn(ref.symbol.owner)
          case _ => false
        }
      case nw: New =>
        isOuter(nw.tpe.classSymbol.owner.enclosingClass)
      case _ =>
        false
    }
  }

  private final val Hoistable = Method | Lazy | Module

  /** The outer prefix implied by type `tpe` */
  private def outerPrefix(tpe: Type)(implicit ctx: Context): Type = tpe match {
    case tpe: TypeRef =>
      tpe.symbol match {
        case cls: ClassSymbol =>
          if (tpe.prefix eq NoPrefix) cls.owner.enclosingClass.thisType
          else tpe.prefix
        case _ =>
          outerPrefix(tpe.underlying)
      }
    case tpe: TypeProxy =>
      outerPrefix(tpe.underlying)
  }

  def outer(implicit ctx: Context): OuterOps = new OuterOps(ctx)

  /** The operations in this class
   *   - add outer parameters
   *   - pass outer arguments to these parameters
   *   - replace outer this references by outer paths.
   *  They are called from erasure. There are two constraints which
   *  suggest these operations should be done in erasure.
   *   - Replacing this references with outer paths loses aliasing information,
   *     so programs will not typecheck with unerased types unless a lot of type
   *     refinements are added. Therefore, outer paths should be computed no
   *     earlier than erasure.
   *   - outer parameters should not show up in signatures, so again
   *     they cannot be added before erasure.
   *   - outer arguments need access to outer parameters as well as to the
   *     original type prefixes of types in New expressions. These prefixes
   *     get erased during erasure. Therefore, outer arguments have to be passed
   *     no later than erasure.
   */
  class OuterOps(val ictx: Context) extends AnyVal {
    private implicit def ctx: Context = ictx

    /** If `cls` has an outer parameter add one to the method type `tp`. */
    def addParam(cls: ClassSymbol, tp: Type): Type =
      if (hasOuterParam(cls)) {
        val mt @ MethodType(pnames, ptypes) = tp
        mt.derivedMethodType(
          nme.OUTER :: pnames, cls.owner.enclosingClass.typeRef :: ptypes, mt.resultType)
      } else tp

    /** If function in an apply node is a constructor that needs to be passed an
     *  outer argument, the singleton list with the argument, otherwise Nil.
     */
    def args(fun: Tree): List[Tree] = {
      if (fun.symbol.isConstructor) {
        val cls = fun.symbol.owner.asClass
        def outerArg(receiver: Tree): Tree = receiver match {
          case New(_) | Super(_, _) =>
            singleton(outerPrefix(receiver.tpe))
          case This(_) =>
            ref(outerParamAccessor(cls)) // will be rewired to outer argument of secondary constructor in phase Constructors
          case TypeApply(Select(r, nme.asInstanceOf_), args) =>
            outerArg(r) // cast was inserted, skip
        }
        if (hasOuterParam(cls))
          methPart(fun) match {
            case Select(receiver, _) => outerArg(receiver).withPos(fun.pos) :: Nil
          }
        else Nil
      } else Nil
    }

    /** The path of outer accessors that references `toCls.this` starting from
     *  the context owner's this node.
     */
    def path(toCls: Symbol): Tree = try {
      def loop(tree: Tree): Tree = {
        val treeCls = tree.tpe.widen.classSymbol
        val outerAccessorCtx = ctx.withPhaseNoLater(ctx.lambdaLiftPhase) // lambdalift mangles local class names, which means we cannot reliably find outer acessors anymore
        ctx.log(i"outer to $toCls of $tree: ${tree.tpe}, looking for ${outerAccName(treeCls.asClass)(outerAccessorCtx)} in $treeCls")
        if (treeCls == toCls) tree
        else loop(tree.select(outerAccessor(treeCls.asClass)(outerAccessorCtx)).ensureApplied)
      }
      ctx.log(i"computing outerpath to $toCls from ${ctx.outersIterator.map(_.owner).toList}")
      loop(This(ctx.owner.enclosingClass.asClass))
    } catch {
      case ex: ClassCastException =>
        throw new ClassCastException(i"no path exists from ${ctx.owner.enclosingClass} to $toCls")
    }

    /** The outer parameter definition of a constructor if it needs one */
    def paramDefs(constr: Symbol): List[ValDef] =
      if (constr.isConstructor && hasOuterParam(constr.owner.asClass)) {
        val MethodType(outerName :: _, outerType :: _) = constr.info
        val outerSym = ctx.newSymbol(constr, outerName, Param, outerType)
        ValDef(outerSym) :: Nil
      }
      else Nil
  }
}
