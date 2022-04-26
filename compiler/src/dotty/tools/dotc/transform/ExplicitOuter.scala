package dotty.tools
package dotc
package transform

import MegaPhase._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Phases._
import core.Types._
import core.Flags._
import core.Decorators._
import core.StdNames.nme
import core.Names._
import core.NameOps._
import SymUtils._
import dotty.tools.dotc.ast.tpd

import collection.mutable
import scala.annotation.tailrec

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
class ExplicitOuter extends MiniPhase with InfoTransformer { thisPhase =>
  import ExplicitOuter._
  import ast.tpd._

  override def phaseName: String = ExplicitOuter.name

  override def description: String = ExplicitOuter.description

  override def runsAfter:         Set[String] = Set(HoistSuperArgs.name)
  override def runsAfterGroupsOf: Set[String] = Set(PatternMatcher.name)

  override def changesMembers: Boolean = true // the phase adds outer accessors

  /** Add outer accessors if a class always needs an outer pointer */
  override def transformInfo(tp: Type, sym: Symbol)(using Context): Type = tp match {
    case tp @ ClassInfo(_, cls, _, decls, _) if needsOuterAlways(cls) =>
      val newDecls = decls.cloneScope
      newOuterAccessors(cls).foreach(newDecls.enter)
      tp.derivedClassInfo(decls = newDecls)
    case _ =>
      tp
  }

  override def infoMayChange(sym: Symbol)(using Context): Boolean = sym.isClass && !sym.is(JavaDefined)

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
  override def transformTemplate(impl: Template)(using Context): Tree = {
    val cls = ctx.owner.asClass
    val isTrait = cls.is(Trait)
    if (needsOuterIfReferenced(cls) &&
        !needsOuterAlways(cls) &&
        impl.existsSubTree(referencesOuter(cls, _)))
      ensureOuterAccessors(cls)

    val clsHasOuter = hasOuter(cls)
    if (clsHasOuter || cls.mixins.exists(needsOuterIfReferenced)) {
      val newDefs = new mutable.ListBuffer[Tree]

      if (clsHasOuter)
        if (isTrait)
          newDefs += DefDef(outerAccessor(cls).asTerm, EmptyTree)
        else {
          val outerParamAcc = outerParamAccessor(cls)
          newDefs += ValDef(outerParamAcc, EmptyTree)
          newDefs += DefDef(outerAccessor(cls).asTerm, ref(outerParamAcc))
        }

      for (parentTrait <- cls.mixins)
        if (needsOuterIfReferenced(parentTrait)) {
          val parentTp = cls.denot.thisType.baseType(parentTrait)
          val outerAccImpl = newOuterAccessor(cls, parentTrait).enteredAfter(thisPhase)
          newDefs += DefDef(outerAccImpl, singleton(fixThis(outerPrefix(parentTp))))
        }

      val parents1 =
        for (parent <- impl.parents) yield
          val parentCls = parent.tpe.classSymbol.asClass
          parent match
            // if we are in a regular class and first parent is also a regular class,
            // make sure we have a contructor
            case parent: TypeTree
            if !cls.is(Trait) && !parentCls.is(Trait) && !defn.NotRuntimeClasses.contains(parentCls) =>
              New(parent.tpe, Nil).withSpan(impl.span)
            case _ => parent
      cpy.Template(impl)(parents = parents1, body = impl.body ++ newDefs)
    }
    else impl
  }

  override def transformClosure(tree: Closure)(using Context): tpd.Tree = {
    if (tree.tpt ne EmptyTree) {
      val cls = tree.tpt.asInstanceOf[TypeTree].tpe.classSymbol
      if (cls.exists && hasOuter(cls.asClass))
        report.error("Not a single abstract method type, requires an outer pointer", tree.srcPos)
    }
    tree
  }
}

object ExplicitOuter {
  import ast.tpd._

  val name: String = "explicitOuter"
  val description: String = "add accessors to outer classes from nested ones"

  /** Ensure that class `cls` has outer accessors */
  def ensureOuterAccessors(cls: ClassSymbol)(using Context): Unit =
    atPhase(explicitOuterPhase.next) {
      if (!hasOuter(cls))
        newOuterAccessors(cls).foreach(_.enteredAfter(explicitOuterPhase.asInstanceOf[DenotTransformer]))
    }

  /** The outer accessor and potentially outer param accessor needed for class `cls` */
  private def newOuterAccessors(cls: ClassSymbol)(using Context) =
    newOuterAccessor(cls, cls) :: (if (cls.is(Trait)) Nil else newOuterParamAccessor(cls) :: Nil)

  /** Scala 2.x and Dotty don't always agree on what should be the type of the outer parameter,
   *  so we replicate the old behavior when passing arguments to methods coming from Scala 2.x.
   */
  private def outerClass(cls: ClassSymbol)(using Context): Symbol = {
    val encl = cls.owner.enclosingClass
    if (cls.is(Scala2x))
      encl.asClass.classInfo.selfInfo match {
        case tp: TypeRef => tp.classSymbol
        case self: Symbol => self
        case _ => encl
      }
    else encl
  }

  /** A new outer accessor or param accessor.
   *  @param  owner  The class where the outer accessor is located
   *  @param  cls    The class relative to which the outer is computed (can be a base class of owner)
   *  @param  name   The name of the outer access
   *  @param  flags  The flags of the outer accessor
   *
   * The type of the outer accessor is computed as follows:
   * Let O[X1, .., Xn] be the class enclosing `cls`.
   *  - if owner == cls, O[X1, ..., Xn]
   *  - otherwise, if the class P enclosing `owner` derives from O, the
   *    base type of P.this wrt class O
   *  - otherwise O[?, ..., ?]
   */
  private def newOuterSym(owner: ClassSymbol, cls: ClassSymbol, name: TermName, flags: FlagSet)(using Context) = {
    val outerThis = owner.owner.enclosingClass.thisType
    val outerCls = outerClass(cls)
    val prefix = owner.thisType.baseType(cls).normalizedPrefix
    val target =
      if (owner == cls)
        outerCls.appliedRef
      else
        outerThis.baseType(outerCls).orElse(
          if prefix == NoPrefix then outerCls.typeRef.appliedTo(outerCls.typeParams.map(_ => TypeBounds.empty))
          else prefix.widen)
    val info = if (flags.is(Method)) ExprType(target) else target
    atPhaseNoEarlier(explicitOuterPhase.next) { // outer accessors are entered at explicitOuter + 1, should not be defined before.
      newSymbol(owner, name, SyntheticArtifact | flags, info, coord = cls.coord)
    }
  }

  /** A new param accessor for the outer field in class `cls` */
  private def newOuterParamAccessor(cls: ClassSymbol)(using Context) =
    newOuterSym(cls, cls, nme.OUTER, Private | Local | ParamAccessor)

  /** A new outer accessor for class `cls` which is a member of `owner` */
  private def newOuterAccessor(owner: ClassSymbol, cls: ClassSymbol)(using Context) = {
    val deferredIfTrait = if (owner.is(Trait)) Deferred else EmptyFlags
    val outerAccIfOwn = if (owner == cls) OuterAccessor else EmptyFlags
    newOuterSym(owner, cls, outerAccName(cls),
      Final | Method | StableRealizable | outerAccIfOwn | deferredIfTrait)
  }

  private def outerAccName(cls: ClassSymbol)(using Context): TermName =
    nme.OUTER.expandedName(cls)

  /** Class needs an outer pointer, provided there is a reference to an outer this in it. */
  def needsOuterIfReferenced(cls: ClassSymbol)(using Context): Boolean =
    !(cls.isStatic ||
      cls.owner.enclosingClass.isStaticOwner ||
      cls.is(PureInterface)
     )

  /** Class unconditionally needs an outer pointer. This is the case if
   *  the class needs an outer pointer if referenced and one of the following holds:
   *  - we might not know at all instantiation sites whether outer is referenced or not
   *  - we need to potentially pass along outer to a parent class or trait
   */
  private def needsOuterAlways(cls: ClassSymbol)(using Context): Boolean =
    needsOuterIfReferenced(cls) &&
    (!hasLocalInstantiation(cls) || // needs outer because we might not know whether outer is referenced or not
     cls.mixins.exists(needsOuterIfReferenced) || // needs outer for parent traits
     cls.info.parents.exists(parent => // needs outer to potentially pass along to parent
       needsOuterIfReferenced(parent.classSymbol.asClass)))

  /** Class is only instantiated in the compilation unit where it is defined */
  private def hasLocalInstantiation(cls: ClassSymbol)(using Context): Boolean =
    // Modules are normally locally instantiated, except if they are declared in a trait,
    // in which case they will be instantiated in the classes that mix in the trait.
    cls.owner.ownersIterator.takeWhile(!_.isStatic).exists(_.isTerm)
    || cls.is(Private, butNot = Module)
    || cls.is(Module) && !cls.owner.is(Trait)

  /** The outer parameter accessor of cass `cls` */
  private def outerParamAccessor(cls: ClassSymbol)(using Context): TermSymbol =
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
  def outerAccessor(cls: ClassSymbol)(using Context): Symbol =
    if (cls.isStatic) NoSymbol // fast return to avoid scanning package decls
    else cls.info.member(outerAccName(cls)).suchThat(_.is(OuterAccessor)).symbol orElse
      cls.info.decls.find(_.is(OuterAccessor))

  /** Class has an outer accessor. Can be called only after phase ExplicitOuter. */
  private def hasOuter(cls: ClassSymbol)(using Context): Boolean =
    needsOuterIfReferenced(cls) && outerAccessor(cls).exists

  /** Class constructor needs an outer argument. Can be called only after phase ExplicitOuter. */
  def needsOuterParam(cls: ClassSymbol)(using Context): Boolean =
    !cls.is(Trait) && needsOuterIfReferenced(cls) && (
      cls.is(JavaDefined) || // java inner class doesn't has outer accessor
      outerAccessor(cls).exists)

  /** Tree references an outer class of `cls` which is not a static owner.
   */
  def referencesOuter(cls: Symbol, tree: Tree)(using Context): Boolean = {
    def isOuterSym(sym: Symbol) =
      !sym.isStaticOwner && cls.isProperlyContainedIn(sym)
    def isOuterRef(ref: Type): Boolean = ref match {
      case ref: ThisType =>
        isOuterSym(ref.cls)
      case ref: TermRef =>
        if (ref.prefix ne NoPrefix)
          !ref.symbol.isStatic && isOuterRef(ref.prefix)
        else (
          ref.symbol.isOneOf(HoistableFlags) &&
            // ref.symbol will be placed in enclosing class scope by LambdaLift, so it might need
            // an outer path then.
            isOuterSym(ref.symbol.owner.enclosingClass)
          ||
            // If not hoistable, ref.symbol will get a proxy in immediately enclosing class. If this properly
            // contains the current class, it needs an outer path.
            // If the symbol is hoistable, it might have free variables for which the same
            // reasoning applies. See pos/i1664.scala
            ctx.owner.enclosingClass.owner.enclosingClass.isContainedIn(ref.symbol.owner)
          )
      case _ => false
    }
    def hasOuterPrefix(tp: Type): Boolean = tp.stripped match {
      case AppliedType(tycon, _) => hasOuterPrefix(tycon)
      case TypeRef(prefix, _) => isOuterRef(prefix)
      case _ => false
    }
    def containsOuterRefs(tp: Type): Boolean = tp match
      case tp: SingletonType => isOuterRef(tp)
      case tp: AndOrType => containsOuterRefs(tp.tp1) || containsOuterRefs(tp.tp2)
      case _ => false
    tree match {
      case _: This | _: Ident => isOuterRef(tree.tpe)
      case nw: New =>
        val newCls = nw.tpe.classSymbol
        isOuterSym(newCls.owner.enclosingClass) ||
        hasOuterPrefix(nw.tpe) ||
        newCls.owner.isTerm && cls.isProperlyContainedIn(newCls)
          // newCls might get proxies for free variables. If current class is
          // properly contained in newCls, it needs an outer path to newCls access the
          // proxies and forward them to the new instance.
      case app: TypeApply if app.symbol.isTypeTest =>
        // Type tests of singletons translate to `eq` tests with references, which might require outer pointers
        containsOuterRefs(app.args.head.tpe)
      case _ =>
        false
    }
  }

  private final val HoistableFlags = Method | Lazy | Module

  /** The outer prefix implied by type `tpe` */
  private def outerPrefix(tpe: Type)(using Context): Type = tpe match {
    case tpe: TypeRef =>
      tpe.symbol match {
        case cls: ClassSymbol =>
          if (tpe.prefix eq NoPrefix) cls.owner.enclosingClass.thisType
          else tpe.prefix
        case _ =>
          // Need to be careful to dealias before erasure, otherwise we lose prefixes.
          atPhaseNoLater(erasurePhase)(outerPrefix(tpe.underlying))
      }
    case tpe: TypeProxy =>
      outerPrefix(tpe.underlying)
  }

  /** It's possible (i1755.scala gives an example) that the type
   *  given by outerPrefix contains a This-reference to a module outside
   *  the context where that module is defined. This needs to be translated
   *  to an access to the module object from the enclosing class or object.
   *
   *  This solution is a bit of a hack; it would be better to avoid
   *  such references to the This of a module from outside the module
   *  in the first place. I was not yet able to find out how such references
   *  arise and how to avoid them.
   */
  private def fixThis(tpe: Type)(using Context): Type = tpe match {
    case tpe: ThisType if tpe.cls.is(Module) && !ctx.owner.isContainedIn(tpe.cls) =>
      fixThis(tpe.cls.owner.thisType.select(tpe.cls.sourceModule.asTerm))
    case tpe: TermRef =>
      tpe.derivedSelect(fixThis(tpe.prefix))
    case _ =>
      tpe
  }

  extension (sym: Symbol) def isOuterParamAccessor(using Context): Boolean =
    sym.is(ParamAccessor) && sym.name == nme.OUTER

  def outer(using Context): OuterOps = new OuterOps(ctx)

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
    /** The context of all operations of this class */
    given [Dummy]: Context = ictx

    /** If `cls` has an outer parameter add one to the method type `tp`. */
    def addParam(cls: ClassSymbol, tp: Type): Type =
      if (needsOuterParam(cls)) {
        val mt @ MethodTpe(pnames, ptypes, restpe) = tp
        mt.derivedLambdaType(
          nme.OUTER :: pnames, outerClass(cls).typeRef :: ptypes, restpe)
      }
      else tp

    /** If function in an apply node is a constructor that needs to be passed an
     *  outer argument, the singleton list with the argument, otherwise Nil.
     */
    def args(fun: Tree): List[Tree] =
      if (fun.symbol.isConstructor) {
        val cls = fun.symbol.owner.asClass
        def outerArg(receiver: Tree): Tree = receiver match {
          case New(_) | Super(_, _) =>
            singleton(fixThis(outerPrefix(receiver.tpe)))
          case This(_) =>
            ref(outerParamAccessor(cls)) // will be rewired to outer argument of secondary constructor in phase Constructors
          case TypeApply(Select(r, nme.asInstanceOf_), args) =>
            outerArg(r) // cast was inserted, skip
        }
        if (needsOuterParam(cls))
          methPart(fun) match {
            case Select(receiver, _) => outerArg(receiver).withSpan(fun.span) :: Nil
          }
        else Nil
      }
      else Nil

    /** If the constructors of the given `cls` need to be passed an outer
     *  argument, the singleton list with the argument, otherwise Nil.
     */
    def argsForNew(cls: ClassSymbol, tpe: Type): List[Tree] =
      if (needsOuterParam(cls)) singleton(fixThis(outerPrefix(tpe))) :: Nil
      else Nil

    /** A path of outer accessors starting from node `start`. `start` defaults to the
     *  context owner's this node. There are two alternative conditions that determine
     *  where the path ends:
     *
     *   - if the initial `count` parameter is non-negative: where the number of
     *     outer accessors reaches count.
     *   - if the initial `count` parameter is negative: where the class symbol of
     *     the type of the reached tree matches `toCls`.
     */
    def path(start: Tree = This(ctx.owner.lexicallyEnclosingClass.asClass),
             toCls: Symbol = NoSymbol,
             count: Int = -1): Tree =
      try
        @tailrec def loop(tree: Tree, count: Int): Tree =
          val treeCls = tree.tpe.classSymbol
          report.log(i"outer to $toCls of $tree: ${tree.tpe}, looking for ${atPhaseNoLater(lambdaLiftPhase)(outerAccName(treeCls.asClass))} in $treeCls")
          if (count == 0 || count < 0 && treeCls == toCls) tree
          else
            val enclClass = ctx.owner.lexicallyEnclosingClass.asClass
            val outerAcc = atPhaseNoLater(lambdaLiftPhase) {
              // lambdalift mangles local class names, which means we cannot
              // reliably find outer acessors anymore
              tree match
                case tree: This if tree.symbol == enclClass && !enclClass.is(Trait) =>
                  outerParamAccessor(enclClass)
                case _ =>
                  outerAccessor(treeCls.asClass)
            }
            assert(outerAcc.exists,
                i"failure to construct path from ${ctx.owner.ownersIterator.toList}%/% to `this` of ${toCls.showLocated};\n${treeCls.showLocated} does not have an outer accessor")
            loop(tree.select(outerAcc).ensureApplied, count - 1)

        report.log(i"computing outerpath to $toCls from ${ctx.outersIterator.map(_.owner).toList}")
        loop(start, count)
      catch case ex: ClassCastException =>
        throw new ClassCastException(i"no path exists from ${ctx.owner.enclosingClass} to $toCls")
  }
}
