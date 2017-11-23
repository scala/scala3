package dotty.tools.dotc
package transform
package patmat

import core._
import Types._
import Contexts._
import Flags._
import ast.Trees._
import ast.tpd
import Decorators._
import Symbols._
import StdNames._
import NameOps._
import Constants._
import typer._
import Applications._
import Inferencing._
import ProtoTypes._
import transform.SymUtils._
import reporting.diagnostic.messages._
import config.Printers.exhaustivity

object SpaceEngine {
  private sealed trait Implementability {
    def show(implicit ctx: Context) = this match {
      case SubclassOf(classSyms) => s"SubclassOf(${classSyms.map(_.show)})"
      case other => other.toString
    }
  }
  private case object ClassOrTrait extends Implementability
  private case class SubclassOf(classSyms: List[Symbol]) extends Implementability
  private case object Unimplementable extends Implementability


  /** Returns a _product_ list of list - each list in the output
   * contains exactly one element from each list in the input.
   *
   * ```
   * productList(List(
   *   List(a1, a2),
   *   List(b1, b2),
   *   List(c1, c2)
   * )) == List(
   *   List(a1, b1, c1),
   *   List(a1, b1, c2),
   *   List(a1, b2, c1),
   *   ???,
   *   List(a2, b2, c2)
   * )
   * ```
   */
  private def productList[T](ls: List[List[T]]): List[List[T]] = ls match {
    case Nil => List(Nil)
    case hs :: ts =>
      val ats = productList(ts)
      hs.flatMap { h => ats.map(h :: _) }
  }
}

/** Scala implementation of space logic */
class SpaceEngine(implicit ctx: Context) extends SpaceLogic {
  import SpaceEngine._
  import tpd._

  private val scalaSomeClass       = ctx.requiredClass("scala.Some")
  private val scalaSeqFactoryClass = ctx.requiredClass("scala.collection.generic.SeqFactory")
  private val scalaListType        = ctx.requiredClassRef("scala.collection.immutable.List")
  private val scalaNilType         = ctx.requiredModuleRef("scala.collection.immutable.Nil")
  private val scalaConsType        = ctx.requiredClassRef("scala.collection.immutable.::")

  private var indent = 0
  private def indentStr = "|  " * indent
  def doDebug[T](pre: => String, post: (T) => String = (_: T) => "")(thunk: => T): T = {
    val pre0 = pre
    if (pre0.nonEmpty) exhaustivity.println(indentStr + pre)
    var unindented = false
    try {
      indent += 1
      val t = thunk
      indent -= 1
      unindented = true
      val post0 = post(t)
      if (post0.nonEmpty) exhaustivity.println(s"$indentStr$pre0 $post0")
      else exhaustivity.println(indentStr + "-")
      t
    } finally if (!unindented) indent -= 1
  }

  def debug(msg: => String): Unit = {
    exhaustivity.println(indentStr + msg)
  }

  def debugShow(s: ConstrainedSpace): String = {
    s.vec.iterator.map(debugShow).mkString("[", ", ", "]")
  }

  def debugShow(s: Space): String = s match {
    case Typ(c: ConstantType, _) => s"Typ<${c.value.show}>"
    case Typ(tp: TermRef, _) => s"Typ<${tp.symbol.showName}>"
    case Typ(tp, _) => s"Typ<${showType(tp)}>"
    case Prod(tp, _, _, params, full) =>
      val description = if (full) "Prod" else "ProdPartial"
      params.iterator.map(debugShow).mkString(s"$description<${showType(tp)}>(", ", ", ")")
  }

  /** Checks if it's possible to create a trait/class which is a subtype of `tp`.
   *
   * - doesn't handle member collisions (will not declare a type unimplementable because of one)
   * - expects that neither Any nor Object reach it
   *   (this is currently true due to both isSubType and and/or type simplification)
   *
   * See [[intersectUnrelatedAtomicTypes]].
   */
  private def implementability(tp: Type): Implementability = tp.dealias match {
    case AndType(tp1, tp2) =>
      (implementability(tp1), implementability(tp2)) match {
        case (Unimplementable, _) | (_, Unimplementable) => Unimplementable
        case (SubclassOf(classSyms1), SubclassOf(classSyms2)) =>
          (for {
            sym1 <- classSyms1
            sym2 <- classSyms2
            result <-
              if (sym1 isSubClass sym2) List(sym1)
              else if (sym2 isSubClass sym1) List(sym2)
              else Nil
          } yield result) match {
            case Nil => Unimplementable
            case lst => SubclassOf(lst)
          }
        case (ClassOrTrait, ClassOrTrait) => ClassOrTrait
        case (SubclassOf(clss), _) => SubclassOf(clss)
        case (_, SubclassOf(clss)) => SubclassOf(clss)
      }
    case OrType(tp1, tp2) =>
      (implementability(tp1), implementability(tp2)) match {
        case (ClassOrTrait, _) | (_, ClassOrTrait) => ClassOrTrait
        case (SubclassOf(classSyms1), SubclassOf(classSyms2)) =>
          SubclassOf(classSyms1 ::: classSyms2)
        case (SubclassOf(classSyms), _) => SubclassOf(classSyms)
        case (_, SubclassOf(classSyms)) => SubclassOf(classSyms)
        case _ => Unimplementable
      }
    case _: SingletonType =>
      // singleton types have no instantiable subtypes
      Unimplementable
    case tp: RefinedType =>
      // refinement itself is not considered - it would at most make
      // a type unimplementable because of a member collision
      implementability(tp.parent)
    case other =>
      val classSym = other.classSymbol
      if (classSym.exists) {
        if (classSym is Final) Unimplementable
        else if (classSym is Trait) ClassOrTrait
        else SubclassOf(List(classSym))
      } else {
        // if no class symbol exists, conservatively say that anything
        // can implement `tp`
        ClassOrTrait
      }
  }

  override def intersectUnrelatedAtomicTypes(tp1: Type, tp2: Type): Option[Space] = {
    val and = AndType(tp1, tp2)
    // Precondition: !(tp1 <:< tp2) && !(tp2 <:< tp1)
    // Then, no leaf of the and-type tree `and` is a subtype of `and`.
    // Then, to create a value of type `and` you must instantiate a trait (class/module)
    // which is a subtype of all the leaves of `and`.
    val imp = implementability(and)

    debug(s"atomic intersection: ${and.show} ~ ${imp.show}")

    imp match {
      case Unimplementable => None
      case _ => Some(Typ(and, true))
    }
  }

  /* Whether the extractor is irrefutable */
  def irrefutable(unapp: tpd.Tree): Boolean = {
    // TODO: optionless patmat
    unapp.tpe.widen.finalResultType.isRef(scalaSomeClass) ||
      (unapp.symbol.is(Synthetic) && unapp.symbol.owner.linkedClass.is(Case)) ||
      productArity(unapp.tpe.widen.finalResultType) > 0
  }

  /** Returns a list of [[ConstrainedSpace]]s representing `_case` pattern.
   *
   * The returned list has more than one element if the pattern contains or-patterns anywhere.
   */
  def project(_case: tpd.CaseDef): List[ConstrainedSpace] = {
    val spaces = projectPattern(_case.pat)
    val termConstraints = projectGuard(_case.guard)
    spaces.map { s =>
      ConstrainedSpace(List(s), termConstraints, Nil)
    }
  }

  def projectPattern(pat: Tree): List[Space] = pat match {
    case Literal(c) => c.value match {
      case s: Symbol => List(Typ(s.termRef))
      case _ => List(Typ(ConstantType(c)))
    }
    case _: BackquotedIdent => List(Typ(pat.tpe))
    case Ident(_) | Select(_, _) =>
      List(Typ(pat.tpe.stripAnnots))
    case Alternative(trees) => trees.flatMap(projectPattern)
    case Bind(_, pat) => projectPattern(pat)
    case UnApply(fun, _, pats) =>
      if (fun.symbol.name == nme.unapplySeq)
        if (fun.symbol.owner == scalaSeqFactoryClass)
          projectSeqPattern(pats)
        else projectSeqPattern(pats).map { s =>
          Prod(pat.tpe.stripAnnots, fun.tpe.widen, fun.symbol, List(s), irrefutable(fun))
        }
      else productList(pats.map(projectPattern)).map { spaces =>
        Prod(pat.tpe.stripAnnots, fun.tpe.widen, fun.symbol, spaces, irrefutable(fun))
      }
    case Typed(pat @ UnApply(_, _, _), _) => projectPattern(pat)
    case Typed(expr, _) =>
      List(Typ(erase(expr.tpe.stripAnnots), true))
    case _ =>
      debug(s"unknown pattern: $pat")
      Nil
  }

  def projectGuard(guard: Tree): List[TermConstraint] = guard match {
    case empty if empty.isEmpty => Nil
    case Literal(c) if c.value.isInstanceOf[Boolean] =>
      if (c.booleanValue) List(AlwaysSatisfied) else List(AlwaysSatisfied.neg)
    case _ => Nil // scalac gives up when guard can't be constant folded
    // TODO: return this if a flag is toggled on to emit more accurate warnings
    // List(Dummy)
  }

  /* Erase a type binding according to erasure semantics in pattern matching */
  def erase(tp: Type): Type = tp match {
    case tp @ AppliedType(tycon, args) =>
      if (tycon.isRef(defn.ArrayClass)) tp.derivedAppliedType(tycon, args.map(erase))
      else tp.derivedAppliedType(tycon, args.map(t => WildcardType))
    case OrType(tp1, tp2) =>
      OrType(erase(tp1), erase(tp2))
    case AndType(tp1, tp2) =>
      AndType(erase(tp1), erase(tp2))
    case tp: RefinedType =>
      tp.derivedRefinedType(erase(tp.parent), tp.refinedName, WildcardType)
    case _ => tp
  }

  def projectSeqPattern(pats: List[Tree]): List[Space] = {
    if (pats.isEmpty) return List(Typ(scalaNilType))

    val (items, zero) = if (pats.last.tpe.isRepeatedParam)
      (pats.init, Typ(scalaListType.appliedTo(pats.last.tpe.argTypes.head)))
    else
      (pats, Typ(scalaNilType))

    val consTp = scalaConsType.appliedTo(pats.head.tpe.widen)
    val unapplySym = consTp.classSymbol.linkedClass.info.member(nme.unapply).symbol
    val unapplyTp = unapplySym.info.appliedTo(pats.head.tpe.widen)

    productList(items.map(projectPattern)).map { spaces =>
      spaces.foldRight[Space](zero) { (head, tail) =>
        Prod(consTp, unapplyTp, unapplySym, List(head, tail), true)
      }
    }
  }

  /** Is `tp1` a subtype of `tp2`?  */
  def isSubType(tp1: Type, tp2: Type): Boolean = {
    val res = tp1 <:< tp2
    debug(s"${tp1.show} <:< ${tp2.show} = $res")
    res
  }

  def isEqualType(tp1: Type, tp2: Type): Boolean = tp1 =:= tp2

  /** Parameter types of the case class type `tp`. Adapted from `unapplyPlan` in patternMatcher  */
  def signature(unapp: Type, unappSym: Symbol, argLen: Int): List[Type] = {
    def caseClass = unappSym.owner.linkedClass

    lazy val caseAccessors = caseClass.caseAccessors.filter(_.is(Method))

    def isSyntheticScala2Unapply(sym: Symbol) =
      sym.is(SyntheticCase) && sym.owner.is(Scala2x)

    val mt @ MethodType(_) = unapp.widen

    // Case unapply:
    // 1. return types of constructor fields if the extractor is synthesized for Scala2 case classes & length match
    // 2. return Nil if unapply returns Boolean  (boolean pattern)
    // 3. return product selector types if unapply returns a product type (product pattern)
    // 4. return product selectors of `T` where `def get: T` is a member of the return type of unapply & length match (named-based pattern)
    // 5. otherwise, return `T` where `def get: T` is a member of the return type of unapply
    //
    // Case unapplySeq:
    // 1. return the type `List[T]` where `T` is the element type of the unapplySeq return type `Seq[T]`

    val sig =
      if (isSyntheticScala2Unapply(unappSym) && caseAccessors.length == argLen)
        caseAccessors.map(_.info.asSeenFrom(mt.paramInfos.head, caseClass).widen)
      else if (mt.finalResultType.isRef(defn.BooleanClass))
        List()
      else {
        val isUnapplySeq = unappSym.name == nme.unapplySeq
        if (isProductMatch(mt.finalResultType, argLen) && !isUnapplySeq) {
          productSelectors(mt.finalResultType).take(argLen)
            .map(_.info.asSeenFrom(mt.finalResultType, mt.resultType.classSymbol).widen)
        }
        else {
          val resTp = mt.finalResultType.select(nme.get).finalResultType.widen
          if (isUnapplySeq) scalaListType.appliedTo(resTp.argTypes.head) :: Nil
          else if (argLen == 0) Nil
          else if (isProductMatch(resTp, argLen))
            productSelectors(resTp).map(_.info.asSeenFrom(resTp, resTp.classSymbol).widen)
          else resTp :: Nil
        }
      }

    debug(s"signature of ${unappSym.showFullName} ----> ${sig.map(_.show).mkString(", ")}")

    sig
  }

  def gadtConstraints(parentTp: Type, childTp: Type): List[TypeConstraint] = {
    val candidates = parentTp.classSymbol.typeParams
    val appliedInfos = childTp.baseType(parentTp.classSymbol).argInfos

    candidates.iterator.zip(appliedInfos.iterator).flatMap {
      case (sym, info) if sym.variance == 0 =>
        Some(TypeEquality(sym.typeRef, info))
      case _ => None
    }.toList
  }

  /** Decompose a type into subspaces -- assume the type can be decomposed */
  def decompose(a: ConstrainedSpace): List[ConstrainedSpace] = {
    val (Typ(tp, _)) :: t = a.vec

    val children = tp.classSymbol.children

    debug(s"candidates for ${tp.show} : [${children.map(_.show).mkString(", ")}]")

    tp.dealias match {
      case AndType(tp1, tp2) =>
        intersect(
          a.withVec(List(Typ(tp1, true))),
          a.withVec(List(Typ(tp2, true)))
        ).map { c => c.withVec(c.vec ::: t)}

      case OrType(tp1, tp2) =>
        List(
          a.withVec(Typ(tp1, true) :: t),
          a.withVec(Typ(tp2, true) :: t)
        )

      case tp if tp.isRef(defn.BooleanClass) =>
        List(
          a.withVec(Typ(ConstantType(Constant(true)), true) :: t),
          a.withVec(Typ(ConstantType(Constant(false)), true) :: t)
        )

      case tp if tp.classSymbol.is(Enum) =>
        children.map { sym =>
          a.withVec(Typ(sym.termRef, true) :: t)
        }

      case tp =>
        val parts = children.map { sym =>
          if (sym.is(ModuleClass))
            refine(tp, sym.sourceModule)
          else
            refine(tp, sym)
        } filter(_.exists)

        debug(s"${tp.show} decomposes to [${parts.map(_.show).mkString(", ")}]")

        parts.map { childTp =>
          val constrs = gadtConstraints(tp, childTp)

          a.copy(
            vec = Typ(childTp, true) :: t,
            typeConstraints = constrs ::: a.typeConstraints
          )
        }
    }
  }

  /** Refine child based on parent
   *
   *  In child class definition, we have:
   *
   *      class Child[Ts] extends path.Parent[Us] with Es
   *      object Child extends path.Parent[Us] with Es
   *      val child = new path.Parent[Us] with Es           // enum values
   *
   *  Given a parent type `parent` and a child symbol `child`, we infer the prefix
   *  and type parameters for the child:
   *
   *      prefix.child[Vs] <:< parent
   *
   *  where `Vs` are fresh type variables and `prefix` is the symbol prefix with all
   *  non-module and non-package `ThisType` replaced by fresh type variables.
   *
   *  If the subtyping is true, the instantiated type `p.child[Vs]` is
   *  returned. Otherwise, `NoType` is returned.
   *
   */
  def refine(parent: Type, child: Symbol): Type = {
    if (child.isTerm && child.is(Case, butNot = Module)) return child.termRef // enum vals always match

    val childTp  = if (child.isTerm) child.termRef else child.typeRef

    val resTp = instantiate(childTp, parent)(ctx.fresh.setNewTyperState())

    if (!resTp.exists)  {
      debug(s"[refine] unqualified child ousted: ${childTp.show} !< ${parent.show}")
      NoType
    }
    else {
      debug(s"$child instantiated ------> $resTp")
      resTp.dealias
    }
  }

  /** Instantiate type `tp1` to be a subtype of `tp2`
   *
   *  Return the instantiated type if type parameters and this type
   *  in `tp1` can be instantiated such that `tp1 <:< tp2`.
   *
   *  Otherwise, return NoType.
   *
   */
  def instantiate(tp1: Type, tp2: Type)(implicit ctx: Context): Type = {
    // map `ThisType` of `tp1` to a type variable
    // precondition: `tp1` should have the shape `path.Child`, thus `ThisType` is always covariant
    val thisTypeMap = new TypeMap {
      def apply(t: Type): Type = t match {
        case tp @ ThisType(tref) if !tref.symbol.isStaticOwner  =>
          if (tref.symbol.is(Module)) mapOver(tref)
          else newTypeVar(TypeBounds.upper(tp.underlying))
        case _ =>
          mapOver(t)
      }
    }

    // replace type parameter references with bounds
    val typeParamMap = new TypeMap {
      def apply(t: Type): Type = t match {
        case tp: TypeRef if tp.symbol.is(TypeParam) && tp.underlying.isInstanceOf[TypeBounds] =>
          // See tests/patmat/gadt.scala  tests/patmat/exhausting.scala  tests/patmat/t9657.scala
          val exposed =
            if (variance == 0) newTypeVar(tp.underlying.bounds)
            else if (variance == 1) mapOver(tp.underlying.hiBound)
            else mapOver(tp.underlying.loBound)

          debug(s"$tp exposed to =====> $exposed")
          exposed
        case _ =>
          mapOver(t)
      }
    }

    // replace uninstantiated type vars with WildcardType, check tests/patmat/3333.scala
    val instUndetMap = new TypeMap {
      def apply(t: Type): Type = t match {
        case tvar: TypeVar if !tvar.isInstantiated => WildcardType(tvar.origin.underlying.bounds)
        case _ => mapOver(t)
      }
    }

    val force = new ForceDegree.Value(
      tvar => !(ctx.typerState.constraint.entry(tvar.origin) eq tvar.origin.underlying),
      minimizeAll = false
    )

    val tvars = tp1.typeParams.map { tparam => newTypeVar(tparam.paramInfo.bounds) }
    val protoTp1 = thisTypeMap(tp1.appliedTo(tvars))

    if (protoTp1 <:< tp2) {
      if (isFullyDefined(protoTp1, force)) protoTp1
      else instUndetMap(protoTp1)
    }
    else {
      val protoTp2 = typeParamMap(tp2)
      if (protoTp1 <:< protoTp2) {
        if (isFullyDefined(AndType(protoTp1, protoTp2), force)) protoTp1
        else instUndetMap(protoTp1)
      }
      else {
        debug(s"$protoTp1 <:< $protoTp2 = false")
        NoType
      }
    }
  }

  /** Abstract sealed types, or-types, Boolean and Java enums can be decomposed */
  def canDecompose(tp: Type): Boolean =
  doDebug[Boolean](s"decomposable: ${tp.show}", res => s"= $res") {
    val dealiasedTp = tp.dealias
    val res = tp.classSymbol.is(allOf(Abstract, Sealed)) ||
      tp.classSymbol.is(allOf(Trait, Sealed)) ||
      dealiasedTp.isInstanceOf[OrType] ||
      (dealiasedTp.isInstanceOf[AndType] && {
        val and = dealiasedTp.asInstanceOf[AndType]
        canDecompose(and.tp1) || canDecompose(and.tp2)
      }) ||
      tp.isRef(defn.BooleanClass) ||
      tp.classSymbol.is(allOf(Enum, Sealed))  // Enum value doesn't have Sealed flag

    res
  }

  /** Show friendly type name with current scope in mind
   *
   *  E.g.    C.this.B     -->  B     if current owner is C
   *          C.this.x.T   -->  x.T   if current owner is C
   *          X[T]         -->  X
   *          C            -->  C     if current owner is C !!!
   *
   */
  def showType(tp: Type): String = {
    val enclosingCls = ctx.owner.enclosingClass

    def isOmittable(sym: Symbol) =
      sym.isEffectiveRoot || sym.isAnonymousClass || sym.name.isReplWrapperName ||
        ctx.definitions.UnqualifiedOwnerTypes.exists(_.symbol == sym) ||
        sym.showFullName.startsWith("scala.") ||
        sym == enclosingCls || sym == enclosingCls.sourceModule

    def refinePrefix(tp: Type): String = tp match {
      case NoPrefix => ""
      case tp: NamedType if isOmittable(tp.symbol) => ""
      case tp: ThisType => refinePrefix(tp.tref)
      case tp: RefinedType => refinePrefix(tp.parent)
      case tp: NamedType => tp.name.show.stripSuffix("$")
      case tp: TypeVar => refinePrefix(tp.instanceOpt)
      case _ => tp.show
    }

    def refine(tp: Type): String = tp match {
      case tp: RefinedType => refine(tp.parent)
      case tp: AppliedType => refine(tp.typeConstructor)
      case tp: ThisType => refine(tp.tref)
      case tp: NamedType =>
        val pre = refinePrefix(tp.prefix)
        if (tp.name == tpnme.higherKinds) pre
        else if (pre.isEmpty) tp.name.show.stripSuffix("$")
        else pre + "." + tp.name.show.stripSuffix("$")
      case _ => tp.show.stripSuffix("$")
    }

    val text = tp.stripAnnots match {
      case tp: OrType => showType(tp.tp1) + " | " + showType(tp.tp2)
      case tp => refine(tp)
    }

    if (text.isEmpty) enclosingCls.show.stripSuffix("$")
    else text
  }

  /** Display spaces */
  def show(spaces: List[Space]): String = {
    def params(tp: Type): List[Type] = tp.classSymbol.primaryConstructor.info.firstParamTypes

    /** does the companion object of the given symbol have custom unapply */
    def hasCustomUnapply(sym: Symbol): Boolean = {
      val companion = sym.companionModule
      companion.findMember(nme.unapply, NoPrefix, excluded = Synthetic).exists ||
        companion.findMember(nme.unapplySeq, NoPrefix, excluded = Synthetic).exists
    }

    def doShow(s: Space, mergeList: Boolean = false): String = s match {
      case Typ(c: ConstantType, _) => c.value.show
      case Typ(tp: TermRef, _) => tp.symbol.showName
      case Typ(tp, decomposed) =>
        val sym = tp.widen.classSymbol

        if (ctx.definitions.isTupleType(tp))
          params(tp).map(_ => "_").mkString("(", ", ", ")")
        else if (scalaListType.isRef(sym))
          if (mergeList) "_: _*" else "_: List"
        else if (scalaConsType.isRef(sym))
          if (mergeList) "_, _: _*"  else "List(_, _: _*)"
        else if (tp.classSymbol.is(CaseClass) && !hasCustomUnapply(tp.classSymbol))
        // use constructor syntax for case class
          showType(tp) + params(tp).map(_ => "_").mkString("(", ", ", ")")
        else if (decomposed) "_: " + showType(tp)
        else "_"
      case Prod(tp, _, sym, params, _) =>
        if (ctx.definitions.isTupleType(tp))
          "(" + params.map(doShow(_)).mkString(", ") + ")"
        else if (tp.isRef(scalaConsType.symbol))
          if (mergeList) params.map(doShow(_, mergeList)).mkString(", ")
          else params.map(doShow(_, true)).filter(_ != "Nil").mkString("List(", ", ", ")")
        else
          showType(sym.owner.typeRef) + params.map(doShow(_)).mkString("(", ", ", ")")
    }

    spaces.map(doShow(_, false)).distinct.mkString(", ")
  }

  def checkable(tree: Match): Boolean = {
    // Possible to check everything, but be compatible with scalac by default
    def isCheckable(tp: Type): Boolean =
        !tp.hasAnnotation(defn.UncheckedAnnot) && (
          ctx.settings.YcheckAllPatmat.value ||
          tp.typeSymbol.is(Sealed) ||
          tp.isInstanceOf[OrType] ||
          (tp.isInstanceOf[AndType] && {
            val and = tp.asInstanceOf[AndType]
            isCheckable(and.tp1) || isCheckable(and.tp2)
          }) ||
          tp.isRef(defn.BooleanClass) ||
          tp.typeSymbol.is(Enum) ||
          canDecompose(tp) ||
          (defn.isTupleType(tp) && tp.dealias.argInfos.exists(isCheckable(_)))
        )

    val Match(sel, cases) = tree
    val res = isCheckable(sel.tpe.widen.dealiasKeepAnnots)
    debug(s"checkable: ${sel.show} = $res")
    res
  }

  def checkExhaustivity(_match: Match): Unit = {
    val Match(sel, cases) = _match
    val selTyp = sel.tpe.widen.dealias

    val projectedPats = cases.flatMap { x =>
      val spaces = project(x)
      debug(s"${x.pat.show} ====> ${debugShow(spaces)}")
      spaces.map { (_, x.body.pos) }
    }

    val uncoveredSpaces = {
      var currentPossibleScrutinees =
        List(ConstrainedSpace(List(Typ(selTyp, decomposed = true)), Nil, Nil))

      for { (space, pos) <- projectedPats } {
        if (!UseOldRedundancyCheck) {
          val possibleMatches = currentPossibleScrutinees
            .flatMap(intersect(_, space))
            .filterNot(NaiveConstraintChecker.hasUnsatisfiableConstraints)

          if (possibleMatches.isEmpty) {
            ctx.warning(MatchCaseUnreachable(), pos)
          }
        }

        currentPossibleScrutinees = currentPossibleScrutinees
          .flatMap(subtract(_, space))
          .filterNot(NaiveConstraintChecker.hasUnsatisfiableConstraints)
      }

      currentPossibleScrutinees
    }

    if (uncoveredSpaces.nonEmpty) {
      debug("Uncovered spaces:")
      uncoveredSpaces.foreach { _space =>
        debug(s"\t${debugShow(_space)}")
      }

      assert( uncoveredSpaces.forall(_.vec.length == 1) )
      ctx.warning(PatternMatchExhaustivity(uncoveredSpaces.map(show).mkString(", ")), sel.pos)
    } else {
      debug("No uncovered spaces.")
    }
  }

  def checkRedundancy(_match: Match): Unit = if (UseOldRedundancyCheck) {
    val Match(_, cases) = _match
    // ignore selector type for now
    // val selTyp = sel.tpe.widen.dealias

    if (cases.length == 1) return

    // starts from the second, the first can't be redundant
    (1 until cases.length).foreach { i =>
      // in redundancy check, take guard as false in order to soundly approximate
      val prevs: List[ConstrainedSpace] = cases.take(i).flatMap { x =>
        if (x.guard.isEmpty) project(x) else Nil
      }

      val curr = project(cases(i))

      debug(s"---------------reachable? ${debugShow(curr)}")
      debug(s"prev: ${debugShow(prevs)}")

      def doSubtract(as: List[ConstrainedSpace], bs: List[ConstrainedSpace]) =
        bs.foldLeft(as) { (as, b) => as.flatMap(subtract(_, b)) }

      if (doSubtract(curr, prevs).isEmpty) {
        ctx.warning(MatchCaseUnreachable(), cases(i).body.pos)
      }
    }
  }

  val UseOldRedundancyCheck: Boolean = true
}
