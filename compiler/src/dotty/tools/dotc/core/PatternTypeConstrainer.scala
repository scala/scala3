package dotty.tools
package dotc
package core

import Decorators._
import Symbols._
import Types._
import Flags._
import Contexts.ctx
import dotty.tools.dotc.reporting.trace
import config.Feature.migrateTo3
import config.Printers._
import dotty.tools.dotc.core.SymDenotations.NoDenotation

trait PatternTypeConstrainer { self: TypeComparer =>

  /** Derive type and GADT constraints that necessarily follow from a pattern with the given type matching
   *  a scrutinee of the given type.
   *
   *  This function breaks down scrutinee and pattern types into subcomponents between which there must be
   *  a subtyping relationship, and derives constraints from those relationships. We have the following situation
   *  in case of a (dynamic) pattern match:
   *
   *       StaticScrutineeType           PatternType
   *                         \            /
   *                      DynamicScrutineeType
   *
   *  In simple cases, it must hold that `PatternType <: StaticScrutineeType`:
   *
   *            StaticScrutineeType
   *                  |         \
   *                  |          PatternType
   *                  |         /
   *               DynamicScrutineeType
   *
   *  A good example of a situation where the above must hold is when static scrutinee type is the root of an enum,
   *  and the pattern is an unapply of a case class, or a case object literal (of that enum).
   *
   *  In slightly more complex cases, we may need to upcast `StaticScrutineeType`:
   *
   *           SharedPatternScrutineeSuperType
   *                  /         \
   *   StaticScrutineeType       PatternType
   *                  \         /
   *               DynamicScrutineeType
   *
   *  This may be the case if the scrutinee is a singleton type or a path-dependent type. It is also the case
   *  for the following definitions:
   *
   *     trait Expr[T]
   *     trait IntExpr extends Expr[T]
   *     trait Const[T] extends Expr[T]
   *
   *     StaticScrutineeType = Const[T]
   *     PatternType = IntExpr
   *
   *  Union and intersection types are an additional complication - if either scrutinee or pattern are a union type,
   *  then the above relationships only need to hold for the "leaves" of the types.
   *
   *  Finally, if pattern type contains hk-types applied to concrete types (as opposed to type variables),
   *  or either scrutinee or pattern type contain type member refinements, the above relationships do not need
   *  to hold at all. Consider (where `T1`, `T2` are unrelated traits):
   *
   *     StaticScrutineeType = { type T <: T1 }
   *     PatternType = { type T <: T2 }
   *
   *  In the above situation, DynamicScrutineeType can equal { type T = T1 & T2 }, but there is no useful relationship
   *  between StaticScrutineeType and PatternType (nor any of their subcomponents). Similarly:
   *
   *     StaticScrutineeType = Option[T1]
   *     PatternType = Some[T2]
   *
   *  Again, DynamicScrutineeType may equal Some[T1 & T2], and there's no useful relationship between the static
   *  scrutinee and pattern types. This does not apply if the pattern type is only applied to type variables,
   *  in which case the subtyping relationship "heals" the type.
   */
  def constrainPatternType(pat: Type, scrut: Type, forceInvariantRefinement: Boolean = false): Boolean = trace(i"constrainPatternType(${scrutRepr(scrut)}, $pat)", gadts, res => s"$res\ngadt = ${ctx.gadt.debugBoundsDescription}") {
    def recur(pat: Type, scrut: Type): Boolean = {
      def classesMayBeCompatible: Boolean = {
        import Flags._
        val patCls = pat.classSymbol
        val scrCls = scrut.classSymbol
        !patCls.exists || !scrCls.exists || {
          if (patCls.is(Final)) patCls.derivesFrom(scrCls)
          else if (scrCls.is(Final)) scrCls.derivesFrom(patCls)
          else if (!patCls.is(Flags.Trait) && !scrCls.is(Flags.Trait))
            patCls.derivesFrom(scrCls) || scrCls.derivesFrom(patCls)
          else true
        }
      }

      def stripRefinement(tp: Type): Type = tp match {
        case tp: RefinedOrRecType => stripRefinement(tp.parent)
        case tp => tp
      }

      def tryConstrainSimplePatternType(pat: Type, scrut: Type) = {
        val patCls = pat.classSymbol
        val scrCls = scrut.classSymbol
        patCls.exists && scrCls.exists
        && (patCls.derivesFrom(scrCls) || scrCls.derivesFrom(patCls))
        && constrainSimplePatternType(pat, scrut, forceInvariantRefinement)
      }

      def constrainUpcasted(scrut: Type): Boolean = trace(i"constrainUpcasted($scrut)", gadts) {
        // Fold a list of types into an AndType
        def buildAndType(xs: List[Type]): Type = {
          @annotation.tailrec def recur(acc: Type, rem: List[Type]): Type = rem match {
            case Nil => acc
            case x :: rem => recur(AndType(acc, x), rem)
          }
          xs match {
            case Nil => NoType
            case x :: xs => recur(x, xs)
          }
        }

        scrut match {
          case scrut: TypeRef if scrut.symbol.isClass =>
            // consider all parents
            val parents = scrut.parents
            val andType = buildAndType(parents)
            !andType.exists || recur(pat, andType)
          case scrut @ AppliedType(tycon: TypeRef, _) if tycon.symbol.isClass =>
            val patCls = pat.classSymbol
            // find all shared parents in the inheritance hierarchy between pat and scrut
            def allParentsSharedWithPat(tp: Type, tpClassSym: ClassSymbol): List[Symbol] = {
              var parents = tpClassSym.info.parents
              if parents.nonEmpty && parents.head.classSymbol == defn.ObjectClass then
                parents = parents.tail
              parents flatMap { tp =>
                val sym = tp.classSymbol.asClass
                if patCls.derivesFrom(sym) then List(sym)
                else allParentsSharedWithPat(tp, sym)
              }
            }
            val allSyms = allParentsSharedWithPat(tycon, tycon.symbol.asClass)
            val baseClasses = allSyms map scrut.baseType
            val andType = buildAndType(baseClasses)
            !andType.exists || recur(pat, andType)
          case _ =>
            def tryGadtBounds = scrut match {
              case scrut: TypeRef =>
                ctx.gadt.bounds(scrut.symbol) match {
                  case tb: TypeBounds =>
                    val hi = tb.hi
                    recur(pat, hi)
                  case null => true
                }
              case _ => true
            }

            def trySuperType =
              val upcasted: Type = scrut match {
                case scrut: TypeProxy =>
                  scrut.superType
                case _ => NoType
              }
              if (upcasted.exists)
                tryConstrainSimplePatternType(pat, upcasted) || constrainUpcasted(upcasted)
              else true

            tryGadtBounds && trySuperType
        }
      }

      def dealiasDropNonmoduleRefs(tp: Type) = tp.dealias match {
        case tp: TermRef =>
          // we drop TermRefs that don't have a class symbol, as they can't
          // meaningfully participate in GADT reasoning and just get in the way.
          // Their info could, for an example, be an AndType. One example where
          // this is important is an enum case that extends its parent and an
          // additional trait - argument-less enum cases desugar to vals.
          // See run/enum-Tree.scala.
          if tp.classSymbol.exists then tp else tp.info
        case tp => tp
      }

      dealiasDropNonmoduleRefs(scrut) match {
        case OrType(scrut1, scrut2) =>
          either(recur(pat, scrut1), recur(pat, scrut2))
        case AndType(scrut1, scrut2) =>
          recur(pat, scrut1) && recur(pat, scrut2)
        case scrut: RefinedOrRecType =>
          recur(pat, stripRefinement(scrut))
        case scrut => dealiasDropNonmoduleRefs(pat) match {
          case OrType(pat1, pat2) =>
            either(recur(pat1, scrut), recur(pat2, scrut))
          case AndType(pat1, pat2) =>
            recur(pat1, scrut) && recur(pat2, scrut)
          case pat: RefinedOrRecType =>
            recur(stripRefinement(pat), scrut)
          case pat =>
            tryConstrainSimplePatternType(pat, scrut)
            || classesMayBeCompatible && constrainUpcasted(scrut)
        }
      }
    }

    /** Reconstruct subtype constraints for type members of the scrutinee and the pattern.
      *
      * To inference SR constraints for the type members from the scrutinee `p` and the pattern `q`,
      * we first find all the abstract type members of `p`: A₁, A₂, ⋯, Aₖ.
      *
      * Then, for each Aᵢ, if `q` also has a type member labaled `Aᵢ`, we inference SR constraints by calling
      * TypeComparer on the relation `p.Aᵢ <:< q.Aᵢ`.
      * We derive SR constraints for type members of the pattern path `q` similarly.
      *
      * Specially, if for some `Aᵢ`, `p.Aᵢ` is abstract while `q.Aᵢ` is not, we will extract constraints
      * for both directions of the subtype relations (i.e. both `p.Aᵢ <:< q.Aᵢ` and `q.Aᵢ <:< p.Aᵢ`).
      *
      * How we find out and handle the path (`TermRef`) of the scrutinee and pattern.
      *
      * - The path of scrutinee is not directly available in `constrainPatternType`, since the scrutinee type
      *   passed to this function is widened.
      *   To make the path available during GADT reasoning, we save the scrutinee path in `Typer.typedCase`. The scrutinee path will be saved in `ctx.gadt.scrutineePath`.
      *   Note that we have to clear the saved scrutinee path after using by calling `ctx.gadt.resetScrutineePath()`.
      *   This is because `constrainPatternType` may be called multiple times for one nested pattern. For example:
      *
      *     e match
      *       case A(B(a), C(b)) => // ...
      *
      *    We have to reset the scrutinee path after constraining `e` against the top level pattern `A(...)`.
      *
      * - The path of pattern is not available when calling the function, and the symbol of the pattern will only be created after GADT reasoning.
      *   Therefore, we will create `SkolemType` acting as a placeholder for the pattern path, and substitute it with the real pattern path
      *   when it is available later in the typer. We call `ctx.gadt.supplyPatternPath` to do the substitution.
      */
    def constrainTypeMembers = trace(i"constrainTypeMembers(${scrutRepr(scrut)}, $pat)", gadts, res => s"$res\ngadt = ${ctx.gadt.debugBoundsDescription}") {
      import NameKinds.DepParamName
      val realScrutineePath = ctx.gadt.scrutineePath

      /* We reset scrutinee path so that the path will only be used at top level. */
      ctx.gadt.resetScrutineePath()

      val saved = state.nn.constraint
      val savedGadt = ctx.gadt.fresh

      val scrutineePath: TermRef | SkolemType = realScrutineePath match
        case null => SkolemType(scrut)
        case _ => realScrutineePath
      val patternPath: SkolemType = ctx.gadt.createPatternSkolem(pat)

      val registerScrutinee = ctx.gadt.contains(scrutineePath) || ctx.gadt.addToConstraint(scrutineePath)
      val registerPattern = ctx.gadt.addToConstraint(patternPath)   // Pattern path is a freshly-created skolem,
                                                                    // so it will always be un-registered at this point

      /** Reconstruct subtype constraints for a path `p`, given that `p` and `q`
        are cohabitated.

        When do SR for each type member (denoted as `T`) of path p, there are the
        following three cases:

        (1) q does not have number T. In this case we should simply return true.

        (2) q.T is a registered type member. We do SR on p.T <:< q.T, but not
            q.T <:< p.T, since if q.T is also registered then
            `constrainTypeMember(q, p, T)` will also be called, during which
            q.T <:< p.T will be handled.

        (3) q.T is unregistered. We will do SR on p.T <:< q.T and q.T <:< p.T.
      */
      def reconstructSubTypeFor(p: PathType, q: PathType) =
        def processMember(sym: Symbol): Boolean =
          q.member(sym.name).isInstanceOf[NoDenotation.type] || {
            val pType = TypeRef(p, sym)
            val qType = TypeRef(q, sym)

            trace(i"constrainTypeMember $pType >:< $qType", gadts, res => s"$res\ngadt = ${ctx.gadt.debugBoundsDescription}") {
              if ctx.gadt.contains(q, sym) then
                isSubType(pType, qType)
              else
                isSubType(pType, qType) && isSubType(qType, pType)
            }
          }

        ctx.gadt.registeredTypeMembers(p) forall { sym => processMember(sym) }

      /** Reconstruct subtype from the cohabitation between the scrutinee and the
        pattern. */
      def constrainPattern: Boolean = {
        ctx.gadt.recordPathAliasing(scrutineePath, patternPath)

        (!registerPattern || reconstructSubTypeFor(patternPath, scrutineePath))
        && (!registerScrutinee || reconstructSubTypeFor(scrutineePath, patternPath))
      }

      /** Reconstruct subtype when the pattern is an alias to another path.

        For example, consider the following pattern match:

          p match
            case q: r.type =>

        Then we can also reconstruct subtype from the cohabitation of p and r.
        */
      def maybeConstrainPatternAlias: Boolean = pat match {
        case ptPath: TermRef =>
          val registerPtPath = ctx.gadt.contains(ptPath) || ctx.gadt.addToConstraint(ptPath)

          val result =
            (!registerPtPath || reconstructSubTypeFor(ptPath, scrutineePath))
            && (!registerScrutinee || reconstructSubTypeFor(scrutineePath, ptPath))

          ctx.gadt.recordPathAliasing(scrutineePath, ptPath)

          result
        case _ =>
          true
      }

      val res = constrainPattern && maybeConstrainPatternAlias

      if !res then
        constraint = saved
        ctx.gadt.restore(savedGadt)

      res
    }

    recur(pat, scrut) && constrainTypeMembers
  }

  /** Show the scrutinee. Will show the path if available. */
  private def scrutRepr(scrut: Type): String =
    ctx.gadt.scrutineePath match
      case null => scrut.show
      case p: PathType => p.show

  /** Constrain "simple" patterns (see `constrainPatternType`).
   *
   *  This function expects to receive two types (scrutinee and pattern), both
   *  of which have class symbols, one of which is derived from another. If the
   *  type "being derived from" is an applied type, it will 1) "upcast" the
   *  deriving type to an applied type with the same constructor and 2) infer
   *  constraints for the applied types' arguments that follow from both
   *  types being inhabited by one value (the scrutinee).
   *
   *  Importantly, note that the pattern type may contain type variables, which
   *  are used to infer type arguments to Unapply trees.
   *
   *  ## Invariant refinement
   *  Essentially, we say that `D[B] extends C[B]` s.t. refines parameter `A` of `trait C[A]` invariantly if
   *  when `c: C[T]` and `c` is instance of `D`, then necessarily `c: D[T]`. This is violated if `A` is variant:
   *
   *     trait C[+A]
   *     trait D[+B](val b: B) extends C[B]
   *     trait E extends D[Any](0) with C[String]
   *
   *  `E` is a counter-example to the above - if `e: E`, then `e: C[String]` and `e` is instance of `D`, but
   *  it is false that `e: D[String]`! This is a problem if we're constraining a pattern like the below:
   *
   *     def foo[T](c: C[T]): T = c match {
   *       case d: D[t] => d.b
   *     }
   *
   *  It'd be unsound for us to say that `t <: T`, even though that follows from `D[t] <: C[T]`.
   *  Note, however, that if `D` was a final class, we *could* rely on that relationship.
   *  To support typical case classes, we also assume that this relationship holds for them and their parent traits.
   *  This is enforced by checking that classes inheriting from case classes do not extend the parent traits of those
   *  case classes without also appropriately extending the relevant case class
   *  (see `RefChecks#checkCaseClassInheritanceInvariant`).
   */
  def constrainSimplePatternType(patternTp: Type, scrutineeTp: Type, forceInvariantRefinement: Boolean): Boolean = {
    def refinementIsInvariant(tp: Type): Boolean = tp match {
      case tp: SingletonType => true
      case tp: ClassInfo => tp.cls.is(Final) || tp.cls.is(Case)
      case tp: TypeProxy => refinementIsInvariant(tp.superType)
      case _ => false
    }

    def widenVariantParams(tp: Type) = tp match {
      case tp @ AppliedType(tycon, args) =>
        val args1 = args.zipWithConserve(tycon.typeParams)((arg, tparam) =>
          if (tparam.paramVarianceSign != 0) TypeBounds.empty else arg
        )
        tp.derivedAppliedType(tycon, args1)
      case tp =>
        tp
    }

    val patternCls = patternTp.classSymbol
    val scrutineeCls = scrutineeTp.classSymbol

    // NOTE: we already know that there is a derives-from relationship in either direction
    val upcastPattern =
      patternCls.derivesFrom(scrutineeCls)

    val pt = if upcastPattern then patternTp.baseType(scrutineeCls) else patternTp
    val tp = if !upcastPattern then scrutineeTp.baseType(patternCls) else scrutineeTp

    val assumeInvariantRefinement =
      migrateTo3 || forceInvariantRefinement || refinementIsInvariant(patternTp)

    trace(i"constraining simple pattern type $tp >:< $pt", gadts, res => s"$res\ngadt = ${ctx.gadt.debugBoundsDescription}") {
      (tp, pt) match {
        case (AppliedType(tyconS, argsS), AppliedType(tyconP, argsP)) =>
          val saved = state.nn.constraint
          val savedGadt = ctx.gadt.fresh
          val result =
            tyconS.typeParams.lazyZip(argsS).lazyZip(argsP).forall { (param, argS, argP) =>
              val variance = param.paramVarianceSign
              if variance == 0 || assumeInvariantRefinement ||
                // As a special case, when pattern and scrutinee types have the same type constructor,
                // we infer better bounds for pattern-bound abstract types.
                argP.typeSymbol.isPatternBound && patternTp.classSymbol == scrutineeTp.classSymbol
              then
                val TypeBounds(loS, hiS) = argS.bounds
                val TypeBounds(loP, hiP) = argP.bounds
                var res = true
                if variance <  1 then res &&= isSubType(loS, hiP)
                if variance > -1 then res &&= isSubType(loP, hiS)
                res
              else true
            }
          if !result then
            constraint = saved
            ctx.gadt.restore(savedGadt)
          result
        case _ =>
          // Give up if we don't get AppliedType, e.g. if we upcasted to Any.
          // Note that this doesn't mean that patternTp, scrutineeTp cannot possibly
          // be co-inhabited, just that we cannot extract information out of them directly
          // and should upcast.
          false
      }
    }
  }
}
