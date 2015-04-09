package dotty.tools.dotc
package transform

import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransform, TreeTransformer}
import dotty.tools.dotc.ast.{Trees, tpd}
import scala.collection.{ mutable, immutable }
import ValueClasses._
import mutable.ListBuffer
import scala.annotation.tailrec
import core._
import Types._, Contexts._, Constants._, Names._, NameOps._, Flags._, DenotTransformers._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Scopes._, Denotations._
import util.Positions._
import Decorators._
import Symbols._

/** This phase performs the following functions, each of which could be split out in a
 *  mini-phase:
 *
 *  (1) Adds super accessors for all super calls that either
 *  appear in a trait or have as a target a member of some outer class.
 *
 *  (2) Converts parameter fields that have the same name as a corresponding
 *  public parameter field in a superclass to a forwarder to the superclass
 *  field (corresponding = super class field is initialized with subclass field)
 *
 *  (3) Adds protected accessors if the access to the protected member happens
 *  in a class which is not a subclass of the member's owner.
 *
 *  (4) Finally, the phase used to mangle the names of class-members which are
 *  private up to an enclosing non-package class, in order to avoid overriding conflicts.
 *  This is currently disabled, and class-qualified private is deprecated.
 *
 *  It also checks that:
 *
 *  (1) Symbols accessed from super are not abstract, or are overridden by
 *  an abstract override.
 *
 *  (2) If a symbol accessed accessed from super is defined in a real class (not a trait),
 *  there are no abstract members which override this member in Java's rules
 *  (see SI-4989; such an access would lead to illegal bytecode)
 *
 *  (3) Super calls do not go to some synthetic members of Any (see isDisallowed)
 *
 *  (4) Super calls do not go to synthetic field accessors
 *
 *  (5) A class and its companion object do not both define a class or module with the
 *  same name.
 *
 *  TODO: Rename phase to "Accessors" because it handles more than just super accessors
 */
class SuperAccessors extends MacroTransform with IdentityDenotTransformer { thisTransformer =>

  import tpd._

  /** the following two members override abstract members in Transform */
  override def phaseName: String = "superaccessors"

  protected def newTransformer(implicit ctx: Context): Transformer =
    new SuperAccTransformer

  class SuperAccTransformer extends Transformer {

    /** validCurrentOwner arrives undocumented, but I reverse engineer it to be
     *  a flag for needsProtectedAccessor which is false while transforming either
     *  a by-name argument block or a closure.  This excludes them from being
     *  considered able to access protected members via subclassing (why?) which in turn
     *  increases the frequency with which needsProtectedAccessor will be true.
     */
    private var validCurrentOwner = true

    private val accDefs = mutable.Map[Symbol, ListBuffer[Tree]]()

    private def storeAccessorDefinition(clazz: Symbol, tree: Tree) = {
      val buf = accDefs.getOrElse(clazz, sys.error("no acc def buf for " + clazz))
      buf += tree
    }

    /** Turn types which are not methodic into ExprTypes. */
    private def ensureMethodic(tpe: Type)(implicit ctx: Context) = tpe match {
      case tpe: MethodicType => tpe
      case _ => ExprType(tpe)
    }

    private def ensureAccessor(sel: Select)(implicit ctx: Context) = {
      val Select(qual, name) = sel
      val sym                = sel.symbol
      val clazz              = qual.symbol.asClass
      val supername          = name.superName

      val superAcc = clazz.info.decl(supername).suchThat(_.signature == sym.signature).symbol orElse {
        ctx.debuglog(s"add super acc ${sym.showLocated} to $clazz")
        val maybeDeferred = if (clazz is Trait) Deferred else EmptyFlags
        val acc = ctx.newSymbol(
            clazz, supername, SuperAccessor | Private | Artifact | Method | maybeDeferred,
            ensureMethodic(sel.tpe.widenSingleton), coord = sym.coord).enteredAfter(thisTransformer)
        // Diagnostic for SI-7091
        if (!accDefs.contains(clazz))
          ctx.error(s"Internal error: unable to store accessor definition in ${clazz}. clazz.hasPackageFlag=${clazz is Package}. Accessor required for ${sel} (${sel.show})", sel.pos)
        else storeAccessorDefinition(clazz, DefDef(acc, EmptyTree))
        acc
      }

      This(clazz).select(superAcc).withPos(sel.pos)
    }

    private def transformArgs(formals: List[Type], args: List[Tree])(implicit ctx: Context) =
      args.zipWithConserve(formals) {(arg, formal) =>
        formal match {
          case _: ExprType => withInvalidOwner(transform(arg))
          case _ => transform(arg)
        }
      }

    /** Check that a class and its companion object to not both define
     *  a class or module with same name
     */
    private def checkCompanionNameClashes(cls: ClassSymbol)(implicit ctx: Context): Unit =
      if (!(cls.owner is ModuleClass)) {
        val other = cls.owner.linkedClass.info.decl(cls.name)
        if (other.symbol.isClass)
          ctx.error(s"name clash: ${cls.owner} defines $cls" + "\n" +
                    s"and its companion ${cls.owner.companionModule} also defines $other",
                    cls.pos)
      }

    /** Expand all declarations in this class which are private within a class.
     *  Note: It's not sure whether this is the right way. Persumably, we expand
     *  qualified privates to prvent them from overriding or be overridden by
     *  symbols that are defined in classes where the qualified private is not
     *  visible. But it seems a bit dubiuous to do this between type checking
     *  and refchecks.
     */
    def expandQualifiedPrivates(cls: ClassSymbol)(implicit ctx: Context) = {
      val decls = cls.info.decls
      val decls1: MutableScope = newScope
      def needsExpansion(sym: Symbol) =
        sym.privateWithin.isClass &&
          !(sym is Protected) &&
          !(sym.privateWithin is ModuleClass) &&
          !(sym is ExpandedName) &&
          !sym.isConstructor
      val nextCtx = ctx.withPhase(thisTransformer.next)
      for (s <- decls) {
        // !!! hacky to do this by mutation; would be better to do with an infotransformer
        // !!! also, why is this done before pickling?
        if (needsExpansion(s)) {
          ctx.deprecationWarning(s"private qualified with a class has been deprecated, use package enclosing ${s.privateWithin} instead", s.pos)
          /* disabled for now
          decls.openForMutations.unlink(s)
          s.copySymDenotation(name = s.name.expandedName(s.privateWithin))
            .installAfter(thisTransformer)
          decls1.enter(s)(nextCtx)
          ctx.log(i"Expanded ${s.name}, ${s.name(nextCtx)}, sym")
          */
        }
      }
      /* Disabled for now:
      if (decls1.nonEmpty) {
        for (s <- decls)
          if (!needsExpansion(s)) decls1.enter(s)(nextCtx)
        val ClassInfo(pre, _, ps, _, selfInfo) = cls.classInfo
        cls.copySymDenotation(info = ClassInfo(pre, cls, ps, decls1, selfInfo))
          .installAfter(thisTransformer)
      }
      */
    }

    private def transformSuperSelect(sel: Select)(implicit ctx: Context): Tree = {
      val Select(sup @ Super(_, mix), name) = sel
      val sym   = sel.symbol
      assert(sup.symbol.exists, s"missing symbol in $sel: ${sup.tpe}")
      val clazz = sup.symbol.asClass

      if (sym is Deferred) {
        val member = sym.overridingSymbol(clazz)
        if (mix != tpnme.EMPTY ||
            !member.exists ||
            !(member is AbsOverride) && member.isIncompleteIn(clazz))
          ctx.error(
              i"${sym.showLocated} is accessed from super. It may not be abstract unless it is overridden by a member declared `abstract' and `override'",
              sel.pos)
      }
      else if (mix == tpnme.EMPTY && !(sym.owner is Trait))
        // SI-4989 Check if an intermediate class between `clazz` and `sym.owner` redeclares the method as abstract.
        for (intermediateClass <- clazz.info.baseClasses.tail.takeWhile(_ != sym.owner)) {
          val overriding = sym.overridingSymbol(intermediateClass)
          if ((overriding is (Deferred, butNot = AbsOverride)) && !(overriding.owner is Trait))
            ctx.error(
                s"${sym.showLocated} cannot be directly accessed from ${clazz} because ${overriding.owner} redeclares it as abstract",
                sel.pos)

        }
      if (name.isTermName && mix == tpnme.EMPTY &&
          ((clazz is Trait) || clazz != ctx.owner.enclosingClass || !validCurrentOwner))
        ensureAccessor(sel)(ctx.withPhase(thisTransformer.next))
      else sel
    }

    // Disallow some super.XX calls targeting Any methods which would
    // otherwise lead to either a compiler crash or runtime failure.
    private def isDisallowed(sym: Symbol)(implicit ctx: Context) = {
      val d = defn
      import d._
      (sym eq Any_isInstanceOf) ||
      (sym eq Any_asInstanceOf) ||
      (sym eq Any_==) ||
      (sym eq Any_!=) ||
      (sym eq Any_##)
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
      val sym = tree.symbol

      def mayNeedProtectedAccessor(sel: Select, targs: List[Tree], goToSuper: Boolean) =
        if (sym.exists && needsProtectedAccessor(sym, tree.pos)) {
          ctx.debuglog("Adding protected accessor for " + tree)
          transform(makeAccessor(sel, targs))
        }
        else if (goToSuper) super.transform(tree)(ctx.withPhase(thisTransformer.next))
        else tree

      try tree match {
        // Don't transform patterns or strange trees will reach the matcher (ticket #4062)
        // TODO Query `ctx.mode is Pattern` instead.
        case CaseDef(pat, guard, body) =>
          cpy.CaseDef(tree)(pat, transform(guard), transform(body))

        case TypeDef(_, impl: Template) =>
          val cls = sym.asClass
          checkCompanionNameClashes(cls)
          expandQualifiedPrivates(cls)
          super.transform(tree)

        case impl: Template =>

          /** For all parameter accessors
           *
           *      val x: T = ...
           *
           *  if
           *  (1) x is forwarded in the supercall to a parameter that's also named `x`
           *  (2) the superclass parameter accessor for `x` is accessible from the current class to
           *  change the accessor to
           *
           *      def x: T = super.x.asInstanceOf[T]
           *
           *  Do the same also if there are intermediate inaccessible parameter accessor forwarders.
           *  The aim of this transformation is to avoid redundant parameter accessor fields.
           */
          def forwardParamAccessors(stats: List[Tree]): List[Tree] = {
            val (superArgs, superParamNames) = impl.parents match {
              case superCall @ Apply(fn, args) :: _ =>
                fn.tpe.widen match {
                  case MethodType(paramNames, _) => (args, paramNames)
                  case _ => (Nil, Nil)
                }
              case _ => (Nil, Nil)
            }
            def inheritedAccessor(sym: Symbol): Symbol = {
              val candidate = sym.owner.asClass.superClass
                .info.decl(sym.name).suchThat(_ is (ParamAccessor, butNot = Mutable)).symbol
              if (candidate.isAccessibleFrom(currentClass.thisType, superAccess = true)) candidate
              else if (candidate is Method) inheritedAccessor(candidate)
              else NoSymbol
            }
            def forwardParamAccessor(stat: Tree): Tree = {
              stat match {
                case stat: ValDef =>
                  val sym = stat.symbol.asTerm
                  if (sym is (PrivateLocalParamAccessor, butNot = Mutable)) {
                    val idx = superArgs.indexWhere(_.symbol == sym)
                    if (idx >= 0 && superParamNames(idx) == stat.name) { // supercall to like-named parameter
                      val alias = inheritedAccessor(sym)
                      if (alias.exists) {
                        def forwarder(implicit ctx: Context) = {
                          sym.copySymDenotation(initFlags = sym.flags | Method, info = ensureMethodic(sym.info))
                            .installAfter(thisTransformer)
                          val superAcc =
                            Super(This(currentClass), tpnme.EMPTY, inConstrCall = false).select(alias)
                          DefDef(sym, superAcc.ensureConforms(sym.info.widen))
                        }
                        return forwarder(ctx.withPhase(thisTransformer.next))
                      }
                    }
                  }
                case _ =>
              }
              stat
            }
            stats map forwardParamAccessor
          }

          def transformTemplate = {
            val ownStats = new ListBuffer[Tree]
            accDefs(currentClass) = ownStats
            // write super accessors after parameters and type aliases (so
            // that order is stable under pickling/unpickling)
            val (params, rest) = impl.body span {
              case td: TypeDef => !td.isClassDef
              case vd: ValOrDefDef => vd.symbol.flags is ParamAccessor
              case _ => false
            }
            ownStats ++= params
            val rest1 = forwardParamAccessors(transformStats(rest, tree.symbol))
            accDefs -= currentClass
            ownStats ++= rest1
            cpy.Template(impl)(body = ownStats.toList)
          }
          transformTemplate

        case TypeApply(sel @ Select(This(_), name), args) =>
          mayNeedProtectedAccessor(sel, args, goToSuper = false)

        case sel @ Select(qual, name) =>
          def transformSelect = {

            qual match {
              case This(_) =>
                // warn if they are selecting a private[this] member which
                // also exists in a superclass, because they may be surprised
                // to find out that a constructor parameter will shadow a
                // field. See SI-4762.
                /* to be added
                if (settings.lint) {
                  if (sym.isPrivateLocal && sym.paramss.isEmpty) {
                    qual.symbol.ancestors foreach { parent =>
                      parent.info.decls filterNot (x => x.isPrivate || x.isLocalToThis) foreach { m2 =>
                        if (sym.name == m2.name && m2.isGetter && m2.accessed.isMutable) {
                          unit.warning(sel.pos,
                            sym.accessString + " " + sym.fullLocationString + " shadows mutable " + m2.name
                              + " inherited from " + m2.owner + ".  Changes to " + m2.name + " will not be visible within "
                              + sym.owner + " - you may want to give them distinct names.")
                        }
                      }
                    }
                  }
                }
                */

                /*
                 * A trait which extends a class and accesses a protected member
                 *  of that class cannot implement the necessary accessor method
                 *  because its implementation is in an implementation class (e.g.
                 *  Foo$class) which inherits nothing, and jvm access restrictions
                 *  require the call site to be in an actual subclass. So non-trait
                 *  classes inspect their ancestors for any such situations and
                 *  generate the accessors.  See SI-2296.
                 */
                // FIXME - this should be unified with needsProtectedAccessor, but some
                // subtlety which presently eludes me is foiling my attempts.
                val shouldEnsureAccessor = (
                  (currentClass is Trait)
                  && (sym is Protected)
                  && sym.enclosingClass != currentClass
                  && !(sym.owner is PackageClass) // SI-7091 no accessor needed package owned (ie, top level) symbols
                  && !(sym.owner is Trait)
                  && sym.owner.enclosingPackageClass != currentClass.enclosingPackageClass
                  && qual.symbol.info.member(sym.name).exists
                  && !needsProtectedAccessor(sym, tree.pos))
                if (shouldEnsureAccessor) {
                  ctx.log("Ensuring accessor for call to protected " + sym.showLocated + " from " + currentClass)
                  ensureAccessor(sel)
                } else
                  mayNeedProtectedAccessor(sel, Nil, goToSuper = false)

              case Super(_, mix) =>
                if ((sym.isTerm) && !(sym is Method) || (sym is Accessor)) {
                  ctx.error(s"super may be not be used on ${sym.underlyingSymbol}", tree.pos)
                } else if (isDisallowed(sym)) {
                  ctx.error(s"super not allowed here: use this.${name.decode} instead", tree.pos)
                }
                transformSuperSelect(sel)

              case _ =>
                mayNeedProtectedAccessor(sel, Nil, goToSuper = true)
            }
          }
          transformSelect

        case tree: DefDef =>
          cpy.DefDef(tree)(
            rhs = if (isMethodWithExtension(sym)) withInvalidOwner(transform(tree.rhs)) else transform(tree.rhs))

        case TypeApply(sel @ Select(qual, name), args) =>
          mayNeedProtectedAccessor(sel, args, goToSuper = true)

        case Assign(lhs @ Select(qual, name), rhs) =>
          def transformAssign = {
            if ((lhs.symbol is Mutable) &&
              (lhs.symbol is JavaDefined) &&
              needsProtectedAccessor(lhs.symbol, tree.pos)) {
              ctx.debuglog("Adding protected setter for " + tree)
              val setter = makeSetter(lhs)
              ctx.debuglog("Replaced " + tree + " with " + setter)
              transform(Apply(setter, qual :: rhs :: Nil))
            } else
              super.transform(tree)
          }
          transformAssign

        case Apply(fn, args) =>
          val MethodType(_, formals) = fn.tpe.widen
          ctx.atPhase(thisTransformer.next) { implicit ctx =>
            cpy.Apply(tree)(transform(fn), transformArgs(formals, args))
          }

        case _ =>
          super.transform(tree)
      }
      catch {
        case ex : AssertionError =>
          if (sym != null && sym != NoSymbol)
            Console.println("TRANSFORM: " + tree.symbol.sourceFile)

          Console.println("TREE: " + tree)
          throw ex
      }
    }

    private def withInvalidOwner[A](trans: => A): A = {
      val saved = validCurrentOwner
      validCurrentOwner = false
      try trans
      finally validCurrentOwner = saved
    }

    /** Add a protected accessor, if needed, and return a tree that calls
     *  the accessor and returns the same member. The result is already
     *  typed.
     *  TODO why is targs needed? It looks like we can do without.
     */
    private def makeAccessor(tree: Select, targs: List[Tree])(implicit ctx: Context): Tree = {
      val Select(qual, _) = tree
      val sym = tree.symbol.asTerm
      val clazz = hostForAccessorOf(sym, currentClass)
      assert(clazz.exists, sym)
      ctx.debuglog("Decided for host class: " + clazz)

      val accName    = sym.name.protectedAccessorName

      // if the result type depends on the this type of an enclosing class, the accessor
      // has to take an object of exactly this type, otherwise it's more general
      val receiverType = if (isThisType(sym.info.finalResultType)) clazz.thisType else clazz.classInfo.selfType
      val accType = {
        def accTypeOf(tpe: Type): Type = tpe match {
          case tpe: PolyType =>
            tpe.derivedPolyType(tpe.paramNames, tpe.paramBounds, accTypeOf(tpe.resultType))
          case _ =>
            MethodType(receiverType :: Nil)(mt => tpe.substThis(sym.owner.asClass, MethodParam(mt, 0)))
        }
        accTypeOf(sym.info)
      }
      val protectedAccessor = clazz.info.decl(accName).suchThat(_.signature == accType.signature).symbol orElse {
        val newAcc = ctx.newSymbol(
            clazz, accName, Artifact, accType, coord = tree.pos).enteredAfter(thisTransformer)
        val code = polyDefDef(newAcc, trefs => vrefss => {
          val (receiver :: _) :: tail = vrefss
          val base = receiver.select(sym).appliedToTypes(trefs)
          (base /: vrefss)(Apply(_, _))
        })
        ctx.debuglog("created protected accessor: " + code)
        storeAccessorDefinition(clazz, code)
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
    private def makeSetter(tree: Select)(implicit ctx: Context): Tree = {
      val field = tree.symbol.asTerm
      val clazz = hostForAccessorOf(field, currentClass)
      assert(clazz.exists, field)
      ctx.debuglog("Decided for host class: " + clazz)

      val accName = field.name.protectedSetterName
      val accType = MethodType(clazz.classInfo.selfType :: field.info :: Nil, defn.UnitType)
      val protectedAccessor = clazz.info.decl(accName).symbol orElse {
        val newAcc = ctx.newSymbol(
            clazz, accName, Artifact, accType, coord = tree.pos).enteredAfter(thisTransformer)
        val code = DefDef(newAcc, vrefss => {
          val (receiver :: value :: Nil) :: Nil = vrefss
          Assign(receiver.select(field), value).withPos(tree.pos)
        })
        ctx.debuglog("created protected setter: " + code)
        storeAccessorDefinition(clazz, code)
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
      val selfType = host.classInfo.selfType
      def accessibleThroughSubclassing =
        validCurrentOwner && (selfType <:< sym.owner.typeRef) && !clazz.is(Trait)

      val isCandidate = (
           sym.is(Protected)
        && sym.is(JavaDefined)
        && !sym.effectiveOwner.is(Package)
        && !accessibleThroughSubclassing
        && (sym.enclosingPackageClass != currentClass.enclosingPackageClass)
        && (sym.enclosingPackageClass == sym.accessBoundary(sym.enclosingPackageClass))
      )
      def isSelfType = !(host.typeRef <:< selfType) && {
        if (selfType.typeSymbol.is(JavaDefined))
          ctx.restrictionError(s"cannot accesses protected $sym from within $clazz with self type $selfType", pos)
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
          || referencingClass.classInfo.selfType <:< sym.owner.typeRef
          || referencingClass.enclosingPackageClass == sym.owner.enclosingPackageClass) {
        assert(referencingClass.isClass, referencingClass)
        referencingClass
      }
      else if (referencingClass.owner.enclosingClass.exists)
        hostForAccessorOf(sym, referencingClass.owner.enclosingClass.asClass)
      else
        referencingClass

    /** Is 'tpe' the type of a member of an enclosing class? */
    private def isThisType(tpe: Type)(implicit ctx: Context): Boolean = tpe match {
      case tpe: ThisType => !tpe.cls.is(PackageClass)
      case tpe: TypeProxy => isThisType(tpe.underlying)
      case _ => false
    }
  }
}
