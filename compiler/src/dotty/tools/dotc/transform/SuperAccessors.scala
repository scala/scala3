package dotty.tools.dotc
package transform

import dotty.tools.dotc.transform.MegaPhase._
import dotty.tools.dotc.ast.{Trees, tpd}
import scala.collection.{ mutable, immutable }
import ValueClasses._
import scala.annotation.tailrec
import core._
import Types._, Contexts._, Constants._, Names._, NameOps._, Flags._, DenotTransformers._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Scopes._, Denotations._
import util.Positions._
import Decorators._
import NameKinds.{ProtectedAccessorName, ProtectedSetterName, OuterSelectName, SuperAccessorName}
import Symbols._, TypeUtils._, SymUtils._

/** This class performs the following functions:
 *
 *  (1) Adds super accessors for all super calls that either
 *  appear in a trait or have as a target a member of some outer class.
 *
 *  (2) Adds protected accessors if the access to the protected member happens
 *  in a class which is not a subclass of the member's owner.
 *
 *  It also checks that:
 *
 *  (1) Symbols accessed from super are not abstract, or are overridden by
 *  an abstract override.
 *
 *  (2) If a symbol accessed from super is defined in a real class (not a trait),
 *  there are no abstract members which override this member in Java's rules
 *  (see SI-4989; such an access would lead to illegal bytecode)
 *
 *  (3) Super calls do not go to some synthetic members of Any (see isDisallowed)
 *
 *  (4) Super calls do not go to synthetic field accessors
 */
class SuperAccessors(thisPhase: DenotTransformer) {

  import tpd._


    /** Some parts of trees will get a new owner in subsequent phases.
     *  These are value class methods, which will become extension methods.
     *  (By-name arguments used to be included also, but these
     *  don't get a new class anymore, they are just wrapped in a new method).
     *
     *  These regions will have to be treated specially for the purpose
     *  of adding accessors. For instance, super calls from these regions
     *  always have to go through an accessor.
     *
     *  The `invalidEnclClass` field, if different from NoSymbol,
     *  contains the symbol that is not a valid owner.
     */
    private[this] var invalidEnclClass: Symbol = NoSymbol

    private def withInvalidCurrentClass[A](trans: => A)(implicit ctx: Context): A = {
      val saved = invalidEnclClass
      invalidEnclClass = ctx.owner
      try trans
      finally invalidEnclClass = saved
    }

    private def validCurrentClass(implicit ctx: Context): Boolean =
      ctx.owner.enclosingClass != invalidEnclClass

    /** List buffers for new accessor definitions, indexed by class */
    private val accDefs = newMutableSymbolMap[mutable.ListBuffer[Tree]]

    /** A super accessor call corresponding to `sel` */
    private def superAccessorCall(sel: Select)(implicit ctx: Context) = {
      val Select(qual, name) = sel
      val sym = sel.symbol
      val clazz = qual.symbol.asClass
      var superName = SuperAccessorName(name.asTermName)
      if (clazz is Trait) superName = superName.expandedName(clazz)
      val superInfo = sel.tpe.widenSingleton.ensureMethodic

      val accPos = sel.pos.focus
      val superAcc = clazz.info.decl(superName)
        .suchThat(_.signature == superInfo.signature).symbol
        .orElse {
          ctx.debuglog(s"add super acc ${sym.showLocated} to $clazz")
          val maybeDeferred = if (clazz is Trait) Deferred else EmptyFlags
          val acc = ctx.newSymbol(
              clazz, superName, Artifact | Method | maybeDeferred,
              superInfo, coord = accPos).enteredAfter(thisPhase)
          // Diagnostic for SI-7091
          if (!accDefs.contains(clazz))
            ctx.error(s"Internal error: unable to store accessor definition in ${clazz}. clazz.hasPackageFlag=${clazz is Package}. Accessor required for ${sel} (${sel.show})", sel.pos)
          else accDefs(clazz) += DefDef(acc, EmptyTree).withPos(accPos)
          acc
        }

      This(clazz).select(superAcc).withPos(sel.pos)
    }

    /** Check selection `super.f` for conforming to rules. If necessary,
     *  replace by a super accessor call.
     */
    private def transformSuperSelect(sel: Select)(implicit ctx: Context): Tree = {
      val Select(sup @ Super(_, mix), name) = sel
      val sym   = sel.symbol
      assert(sup.symbol.exists, s"missing symbol in $sel: ${sup.tpe}")
      val clazz = sup.symbol

      if (sym.isTerm && !sym.is(Method, butNot = Accessor) && !ctx.owner.isParamForwarder)
        // ParamForwaders as installed ParamForwarding.scala do use super calls to vals
        ctx.error(s"super may be not be used on ${sym.underlyingSymbol}", sel.pos)
      else if (isDisallowed(sym))
        ctx.error(s"super not allowed here: use this.${sel.name} instead", sel.pos)
      else if (sym is Deferred) {
        val member = sym.overridingSymbol(clazz.asClass)
        if (!mix.name.isEmpty ||
            !member.exists ||
            !((member is AbsOverride) && member.isIncompleteIn(clazz)))
          ctx.error(
              i"${sym.showLocated} is accessed from super. It may not be abstract unless it is overridden by a member declared `abstract' and `override'",
              sel.pos)
        else ctx.log(i"ok super $sel ${sym.showLocated} $member $clazz ${member.isIncompleteIn(clazz)}")
      }
      else if (mix.name.isEmpty && !(sym.owner is Trait))
        // SI-4989 Check if an intermediate class between `clazz` and `sym.owner` redeclares the method as abstract.
        for (intermediateClass <- clazz.info.baseClasses.tail.takeWhile(_ != sym.owner)) {
          val overriding = sym.overridingSymbol(intermediateClass)
          if ((overriding is (Deferred, butNot = AbsOverride)) && !(overriding.owner is Trait))
            ctx.error(
                s"${sym.showLocated} cannot be directly accessed from ${clazz} because ${overriding.owner} redeclares it as abstract",
                sel.pos)

        }
      if (name.isTermName && mix.name.isEmpty &&
          ((clazz is Trait) || clazz != ctx.owner.enclosingClass || !validCurrentClass))
        superAccessorCall(sel)(ctx.withPhase(thisPhase.next))
      else sel
    }

    /** Disallow some super.XX calls targeting Any methods which would
     *  otherwise lead to either a compiler crash or runtime failure.
     */
    private def isDisallowed(sym: Symbol)(implicit ctx: Context) =
      sym.isTypeTestOrCast ||
      (sym eq defn.Any_==) ||
      (sym eq defn.Any_!=) ||
      (sym eq defn.Any_##)

    /** Replace `sel` (or `sel[targs]` if `targs` is nonempty) with a protected accessor
     *  call, if necessary.
     */
    private def ensureProtectedAccessOK(sel: Select, targs: List[Tree])(implicit ctx: Context) = {
      val sym = sel.symbol
      if (sym.isTerm && !sel.name.is(OuterSelectName) && needsProtectedAccessor(sym, sel.pos)) {
        ctx.debuglog("Adding protected accessor for " + sel)
        protectedAccessorCall(sel, targs)
      } else sel
    }

    /** Add a protected accessor, if needed, and return a tree that calls
     *  the accessor and returns the same member. The result is already
     *  typed.
     */
    private def protectedAccessorCall(sel: Select, targs: List[Tree])(implicit ctx: Context): Tree = {
      val Select(qual, _) = sel
      val sym = sel.symbol.asTerm
      val clazz = hostForAccessorOf(sym, currentClass)
      assert(clazz.exists, sym)
      ctx.debuglog("Decided for host class: " + clazz)

      val accName = ProtectedAccessorName(sym.name)

      // if the result type depends on the this type of an enclosing class, the accessor
      // has to take an object of exactly this type, otherwise it's more general
      val receiverType =
        if (isThisType(sym.info.finalResultType)) clazz.thisType
        else clazz.classInfo.selfType
      val accType = {
        def accTypeOf(tpe: Type): Type = tpe match {
          case tpe: PolyType =>
            tpe.derivedLambdaType(tpe.paramNames, tpe.paramInfos, accTypeOf(tpe.resultType))
          case _ =>
            MethodType(receiverType :: Nil)(mt => tpe.substThis(sym.owner.asClass, mt.newParamRef(0)))
        }
        accTypeOf(sym.info)
      }
      val accPos = sel.pos.focus
      val protectedAccessor = clazz.info.decl(accName).suchThat(_.signature == accType.signature).symbol orElse {
        val newAcc = ctx.newSymbol(
          clazz, accName, Artifact | Method, accType, coord = accPos).enteredAfter(thisPhase)
        val code = polyDefDef(newAcc, trefs => vrefss => {
          val (receiver :: _) :: tail = vrefss
          val base = receiver.select(sym).appliedToTypes(trefs)
          (base /: tail)(Apply(_, _)).withPos(accPos)
        })
        ctx.debuglog("created protected accessor: " + code)
        accDefs(clazz) += code
        newAcc
      }
      val res = This(clazz)
        .select(protectedAccessor)
        .appliedToTypeTrees(targs)
        .appliedTo(qual)
        .withPos(sel.pos)
      ctx.debuglog(s"Replaced $sel with $res")
      res
    }

    def isProtectedAccessor(tree: Tree)(implicit ctx: Context): Boolean = tree match {
      case Apply(TypeApply(Select(_, name), _), qual :: Nil) =>
        name.is(ProtectedAccessorName) || name.is(ProtectedSetterName)
      case _ => false
    }

    /** Add a protected accessor, if needed, and return a tree that calls
     *  the accessor and returns the same member. The result is already
     *  typed.
     */
    private def protectedAccessor(tree: Select, targs: List[Tree])(implicit ctx: Context): Tree = {
      val Select(qual, _) = tree
      val sym = tree.symbol.asTerm
      val clazz = hostForAccessorOf(sym, currentClass)
      assert(clazz.exists, sym)
      ctx.debuglog("Decided for host class: " + clazz)

      val accName = ProtectedAccessorName(sym.name)

      // if the result type depends on the this type of an enclosing class, the accessor
      // has to take an object of exactly this type, otherwise it's more general
      val receiverType =
        if (isThisType(sym.info.finalResultType)) clazz.thisType
        else clazz.classInfo.selfType
      def accTypeOf(tpe: Type): Type = tpe match {
        case tpe: PolyType =>
          tpe.derivedLambdaType(tpe.paramNames, tpe.paramInfos, accTypeOf(tpe.resultType))
        case _ =>
          MethodType(receiverType :: Nil)(mt => tpe.substThis(sym.owner.asClass, mt.newParamRef(0)))
      }
      val accType = accTypeOf(sym.info)
      val accPos = tree.pos.focus
      val protectedAccessor = clazz.info.decl(accName).suchThat(_.signature == accType.signature).symbol orElse {
        val newAcc = ctx.newSymbol(
            clazz, accName, Artifact, accType, coord = accPos).enteredAfter(thisPhase)
        val code = polyDefDef(newAcc, trefs => vrefss => {
          val (receiver :: _) :: tail = vrefss
          val base = receiver.select(sym).appliedToTypes(trefs)
          (base /: vrefss)(Apply(_, _)).withPos(accPos)
        })
        ctx.debuglog("created protected accessor: " + code)
        accDefs(clazz) += code
        newAcc
      }
      val res = This(clazz)
        .select(protectedAccessor)
        .appliedToTypeTrees(targs)
        .appliedTo(qual)
        .withPos(tree.pos)
      ctx.debuglog(s"Replaced $tree with $res")
      res
    }

    /** Add an accessor for field, if needed, and return a selection tree for it .
     *  The result is not typed.
     */
    private def protectedSetter(tree: Select)(implicit ctx: Context): Tree = {
      val field = tree.symbol.asTerm
      val clazz = hostForAccessorOf(field, currentClass)
      assert(clazz.exists, field)
      ctx.debuglog("Decided for host class: " + clazz)

      val accName = ProtectedSetterName(field.name)
      val accType = MethodType(clazz.classInfo.selfType :: field.info :: Nil, defn.UnitType)
      val accPos = tree.pos.focus
      val protectedAccessor = clazz.info.decl(accName).symbol orElse {
        val newAcc = ctx.newSymbol(
            clazz, accName, Artifact | Method, accType, coord = accPos).enteredAfter(thisPhase)
        val code = DefDef(newAcc, vrefss => {
          val (receiver :: value :: Nil) :: Nil = vrefss
          Assign(receiver.select(field), value).withPos(accPos)
        })
        ctx.debuglog("created protected setter: " + code)
        accDefs(clazz) += code
        newAcc
      }
      This(clazz).select(protectedAccessor).withPos(tree.pos)
    }

    /** Does `sym` need an accessor when accessed from `currentClass`?
     *  A special case arises for classes with explicit self-types. If the
     *  self type is a Java class, and a protected accessor is needed, we issue
     *  an error. If the self type is a Scala class, we don't add an accessor.
     *  An accessor is not needed if the access boundary is larger than the
     *  enclosing package, since that translates to 'public' on the host sys.
     *  (as Java has no real package nesting).
     *
     * If the access happens inside a 'trait', access is more problematic since
     * the implementation code is moved to an '$class' class which does not
     * inherit anything. Since we can't (yet) add accessors for 'required'
     * classes, this has to be signaled as error.
     * FIXME Need to better understand this logic
     */
    private def needsProtectedAccessor(sym: Symbol, pos: Position)(implicit ctx: Context): Boolean = {
      val clazz = currentClass
      val host = hostForAccessorOf(sym, clazz)
      def accessibleThroughSubclassing =
        validCurrentClass && clazz.classInfo.selfType.derivesFrom(sym.owner) && !clazz.is(Trait)

      val isCandidate = (
           sym.is(Protected)
        && sym.is(JavaDefined)
        && !sym.effectiveOwner.is(Package)
        && !accessibleThroughSubclassing
        && (sym.enclosingPackageClass != currentClass.enclosingPackageClass)
        && (sym.enclosingPackageClass == sym.accessBoundary(sym.enclosingPackageClass))
      )
      val hostSelfType = host.classInfo.selfType
      def isSelfType = !(host.appliedRef <:< hostSelfType) && {
        if (hostSelfType.typeSymbol.is(JavaDefined))
          ctx.restrictionError(
            s"cannot accesses protected $sym from within $clazz with host self type $hostSelfType", pos)
        true
      }
      def isJavaProtected = host.is(Trait) && sym.is(JavaDefined) && {
        ctx.restrictionError(
          s"""$clazz accesses protected $sym inside a concrete trait method.
             |Add an accessor in a class extending ${sym.enclosingClass} as a workaround.""".stripMargin,
          pos
        )
        true
      }
      isCandidate && !host.is(Package) && !isSelfType && !isJavaProtected
    }

    /** Return the innermost enclosing class C of referencingClass for which either
     *  of the following holds:
     *     - C is a subclass of sym.owner or
     *     - C is declared in the same package as sym's owner
     */
    private def hostForAccessorOf(sym: Symbol, referencingClass: ClassSymbol)(implicit ctx: Context): ClassSymbol =
      if (referencingClass.derivesFrom(sym.owner)
          || referencingClass.classInfo.selfType <:< sym.owner.appliedRef
          || referencingClass.enclosingPackageClass == sym.owner.enclosingPackageClass) {
        assert(referencingClass.isClass, referencingClass)
        referencingClass
      }
      else if (referencingClass.owner.enclosingClass.exists)
        hostForAccessorOf(sym, referencingClass.owner.enclosingClass.asClass)
      else
        referencingClass

    /** Is 'tpe' a ThisType, or a type proxy with a ThisType as transitively underlying type? */
    private def isThisType(tpe: Type)(implicit ctx: Context): Boolean = tpe match {
      case tpe: ThisType => !tpe.cls.is(PackageClass)
      case tpe: TypeProxy => isThisType(tpe.underlying)
      case _ => false
    }

    /** Transform select node, adding super and protected accessors as needed */
    def transformSelect(tree: Tree, targs: List[Tree])(implicit ctx: Context) = {
      val sel @ Select(qual, name) = tree
      val sym = sel.symbol
      qual match {
        case _: This =>
          /*
           * A trait which extends a class and accesses a protected member
           *  of that class cannot implement the necessary accessor method
           *  because its implementation is in an implementation class (e.g.
           *  Foo$class) which inherits nothing, and jvm access restrictions
           *  require the call site to be in an actual subclass. So non-trait
           *  classes inspect their ancestors for any such situations and
           *  generate the accessors.  See SI-2296.
           */
          // FIXME (from scalac's SuperAccessors)
          // - this should be unified with needsProtectedAccessor, but some
          // subtlety which presently eludes me is foiling my attempts.
          val shouldEnsureAccessor = (
            (currentClass is Trait)
            && (sym is Protected)
            && sym.enclosingClass != currentClass
            && !(sym.owner is PackageClass) // SI-7091 no accessor needed package owned (ie, top level) symbols
            && !(sym.owner is Trait)
            && sym.owner.enclosingPackageClass != currentClass.enclosingPackageClass
            && qual.symbol.info.member(sym.name).exists
            && !needsProtectedAccessor(sym, sel.pos))
          if (shouldEnsureAccessor) {
            ctx.log("Ensuring accessor for call to protected " + sym.showLocated + " from " + currentClass)
            superAccessorCall(sel)
          } else
            ensureProtectedAccessOK(sel, targs)

        case Super(_, mix) =>
          transformSuperSelect(sel)

        case _ =>
          ensureProtectedAccessOK(sel, targs)
      }
    }

    /** Transform assignment, adding a protected setter if needed */
    def transformAssign(tree: Tree)(implicit ctx: Context) = {
      val Assign(lhs @ Select(qual, name), rhs) = tree
      if ((lhs.symbol is Mutable) &&
        (lhs.symbol is JavaDefined) &&
        needsProtectedAccessor(lhs.symbol, tree.pos)) {
        ctx.debuglog("Adding protected setter for " + tree)
        val setter = protectedSetter(lhs)
        ctx.debuglog("Replaced " + tree + " with " + setter)
        setter.appliedTo(qual, rhs)
      }
      else tree
    }

    /** Wrap template to template transform `op` with needed initialization and finalization */
    def wrapTemplate(tree: Template)(op: Template => Template)(implicit ctx: Context) = {
      accDefs(currentClass) = new mutable.ListBuffer[Tree]
      val impl = op(tree)
      val accessors = accDefs.remove(currentClass).get
      if (accessors.isEmpty) impl
      else {
        val (params, rest) = impl.body span {
          case td: TypeDef => !td.isClassDef
          case vd: ValOrDefDef => vd.symbol.flags is ParamAccessor
          case _ => false
        }
        cpy.Template(impl)(body = params ++ accessors ++ rest)
      }
    }

    /** Wrap `DefDef` producing operation `op`, potentially setting `invalidClass` info */
    def wrapDefDef(ddef: DefDef)(op: => DefDef)(implicit ctx: Context) =
      if (isMethodWithExtension(ddef.symbol)) withInvalidCurrentClass(op) else op
}
