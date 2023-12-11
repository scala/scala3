package dotty.tools
package dotc
package transform
package patmat

import core.*
import Types.*
import Contexts.*
import Flags.*
import ast.*
import Decorators.{ show => _, * }
import Symbols.*
import StdNames.*
import NameOps.*
import Constants.*
import typer.*
import Applications.*
import Inferencing.*
import ProtoTypes.*
import reporting.*
import config.Printers.{exhaustivity => debug}
import util.{SrcPos, NoSourcePosition}

import scala.annotation.internal.sharable
import scala.collection.mutable

import SpaceEngine.*

/* Space logic for checking exhaustivity and unreachability of pattern matching
 *
 *  Space can be thought of as a set of possible values. A type or a pattern
 *  both refer to spaces. The space of a type is the values that inhabit the
 *  type. The space of a pattern is the values that can be covered by the
 *  pattern.
 *
 *  Space is recursively defined as follows:
 *
 *      1. `Empty` is a space
 *      2. For a type T, `Typ(T)` is a space
 *      3. A union of spaces `S1 | S2 | ...` is a space
 *      4. `Prod(S1, S2, ..., Sn)` is a product space.
 *
 *  For the problem of exhaustivity check, its formulation in terms of space is as follows:
 *
 *      Is the space Typ(T) a subspace of the union of space covered by all the patterns?
 *
 *  The problem of unreachable patterns can be formulated as follows:
 *
 *      Is the space covered by a pattern a subspace of the space covered by previous patterns?
 *
 *  Assumption:
 *    (1) One case class cannot be inherited directly or indirectly by another
 *        case class.
 *    (2) Inheritance of a case class cannot be well handled by the algorithm.
 *
 */

/** space definition */
sealed trait Space:

  @sharable private val isSubspaceCache = mutable.HashMap.empty[Space, Boolean]

  def isSubspace(b: Space)(using Context): Boolean =
    val a = this
    val a2 = a.simplify
    val b2 = b.simplify
    if (a ne a2) || (b ne b2) then a2.isSubspace(b2)
    else if a == Empty then true
    else if b == Empty then false
    else trace(s"isSubspace(${show(this)}, ${show(b)})", debug) {
      isSubspaceCache.getOrElseUpdate(b, computeIsSubspace(a, b))
    }

  @sharable private var mySimplified: Space | Null = null

  def simplify(using Context): Space =
    val simplified = mySimplified
    if simplified == null then
      val simplified = SpaceEngine.computeSimplify(this)
      mySimplified = simplified
      simplified
    else simplified
end Space

/** Empty space */
case object Empty extends Space

/** Space representing the set of all values of a type
 *
 * @param tp: the type this space represents
 * @param decomposed: does the space result from decomposition? Used for pretty print
 *
 */
case class Typ(tp: Type, decomposed: Boolean = true) extends Space:
  private var myDecompose: List[Typ] | Null = null

  def canDecompose(using Context): Boolean = decompose != ListOfTypNoType

  def decompose(using Context): List[Typ] =
    val decompose = myDecompose
    if decompose == null then
      val decompose = tp match
        case Parts(parts) => parts.map(Typ(_, decomposed = true))
        case _            => ListOfTypNoType
      myDecompose = decompose
      decompose
    else decompose
end Typ

/** Space representing an extractor pattern */
case class Prod(tp: Type, unappTp: TermRef, params: List[Space]) extends Space

/** Union of spaces */
case class Or(spaces: Seq[Space]) extends Space

object SpaceEngine {
  import tpd.*

  def simplify(space: Space)(using Context): Space           = space.simplify
  def isSubspace(a: Space, b: Space)(using Context): Boolean = a.isSubspace(b)
  def canDecompose(typ: Typ)(using Context): Boolean         = typ.canDecompose
  def decompose(typ: Typ)(using Context): List[Typ]          = typ.decompose

  /** Simplify space such that a space equal to `Empty` becomes `Empty` */
  def computeSimplify(space: Space)(using Context): Space = trace(s"simplify ${show(space)} --> ", debug, show)(space match {
    case Prod(tp, fun, spaces) =>
      val sps = spaces.mapconserve(simplify)
      if sps.contains(Empty) then Empty
      else if decompose(tp).isEmpty then Empty
      else if sps eq spaces then space else Prod(tp, fun, sps)
    case Or(spaces) =>
      val spaces2 = spaces.map(simplify).filter(_ != Empty)
      if spaces2.isEmpty then Empty
      else if spaces2.lengthIs == 1 then spaces2.head
      else if spaces2.corresponds(spaces)(_ eq _) then space else Or(spaces2)
    case typ: Typ =>
      if decompose(typ).isEmpty then Empty
      else space
    case _ => space
  })

  /** Remove a space if it's a subspace of remaining spaces
   *
   *  Note: `dedup` will return the same result if the sequence >= 10
   */
  def dedup(spaces: Seq[Space])(using Context): Seq[Space] =
    if (spaces.lengthCompare(1) <= 0 || spaces.lengthCompare(10) >= 0) spaces
    else {
      val res = spaces.map(sp => (sp, spaces.filter(_ ne sp))).find {
        case (sp, sps) => isSubspace(sp, Or(LazyList(sps*)))
      }
      if (res.isEmpty) spaces
      else res.get._2
    }

  /** Flatten space to get rid of `Or` for pretty print */
  def flatten(space: Space)(using Context): Seq[Space] = space match {
    case Prod(tp, fun, spaces) =>
      val ss = LazyList(spaces*).map(flatten)

      ss.foldLeft(LazyList(Nil : List[Space])) { (acc, flat) =>
        for { sps <- acc; s <- flat }
        yield sps :+ s
      }.map { sps =>
        Prod(tp, fun, sps)
      }

    case Or(spaces) =>
      LazyList(spaces*).flatMap(flatten)

    case _ =>
      List(space)
  }

  /** Is `a` a subspace of `b`? Equivalent to `simplify(simplify(a) - simplify(b)) == Empty`, but faster */
  def computeIsSubspace(a: Space, b: Space)(using Context): Boolean = {
    val a2 = simplify(a)
    val b2 = simplify(b)
    if (a ne a2) || (b ne b2) then isSubspace(a2, b2)
    else (a, b) match {
      case (Empty, _) => true
      case (_, Empty) => false
      case (Or(ss), _) => ss.forall(isSubspace(_, b))
      case (a @ Typ(tp1, _), Or(ss)) =>  // optimization: don't go to subtraction too early
        ss.exists(isSubspace(a, _))
        || canDecompose(a) && isSubspace(Or(decompose(a)), b)
      case (_, Or(_))  => simplify(minus(a, b)) == Empty
      case (a @ Typ(tp1, _), b @ Typ(tp2, _)) =>
        isSubType(tp1, tp2)
        || canDecompose(a) && isSubspace(Or(decompose(a)), b)
        || canDecompose(b) && isSubspace(a, Or(decompose(b)))
      case (Prod(tp1, _, _), Typ(tp2, _)) =>
        isSubType(tp1, tp2)
      case (a @ Typ(tp1, _), Prod(tp2, fun, ss)) =>
        isSubType(tp1, tp2)
        && covers(fun, tp1, ss.length)
        && isSubspace(Prod(tp2, fun, signature(fun, tp1, ss.length).map(Typ(_, false))), b)
        || canDecompose(a) && isSubspace(Or(decompose(a)), b)
      case (Prod(_, fun1, ss1), Prod(_, fun2, ss2)) =>
        isSameUnapply(fun1, fun2) && ss1.lazyZip(ss2).forall(isSubspace)
    }
  }

  /** Intersection of two spaces  */
  def intersect(a: Space, b: Space)(using Context): Space = trace(s"${show(a)} & ${show(b)}", debug, show) {
    (a, b) match {
      case (Empty, _) | (_, Empty) => Empty
      case (_, Or(ss)) => Or(ss.map(intersect(a, _)).filter(_ ne Empty))
      case (Or(ss), _) => Or(ss.map(intersect(_, b)).filter(_ ne Empty))
      case (a @ Typ(tp1, _), b @ Typ(tp2, _)) =>
        if isSubType(tp1, tp2) then a
        else if isSubType(tp2, tp1) then b
        else if canDecompose(a) then intersect(Or(decompose(a)), b)
        else if canDecompose(b) then intersect(a, Or(decompose(b)))
        else intersectUnrelatedAtomicTypes(tp1, tp2)(a)
      case (a @ Typ(tp1, _), Prod(tp2, fun, ss)) =>
        if isSubType(tp2, tp1) then b
        else if canDecompose(a) then intersect(Or(decompose(a)), b)
        else if isSubType(tp1, tp2) then a // problematic corner case: inheriting a case class
        else intersectUnrelatedAtomicTypes(tp1, tp2)(b)
      case (Prod(tp1, fun, ss), b @ Typ(tp2, _)) =>
        if isSubType(tp1, tp2) then a
        else if canDecompose(b) then intersect(a, Or(decompose(b)))
        else if isSubType(tp2, tp1) then a  // problematic corner case: inheriting a case class
        else intersectUnrelatedAtomicTypes(tp1, tp2)(a)
      case (a @ Prod(tp1, fun1, ss1), Prod(tp2, fun2, ss2)) =>
        if !isSameUnapply(fun1, fun2) then intersectUnrelatedAtomicTypes(tp1, tp2)(a)
        else if ss1.lazyZip(ss2).exists((a, b) => simplify(intersect(a, b)) == Empty) then Empty
        else Prod(tp1, fun1, ss1.lazyZip(ss2).map(intersect))
    }
  }

  /** The space of a not covered by b */
  def minus(a: Space, b: Space)(using Context): Space = trace(s"${show(a)} - ${show(b)}", debug, show) {
    (a, b) match {
      case (Empty, _) => Empty
      case (_, Empty) => a
      case (Or(ss), _) => Or(ss.map(minus(_, b)))
      case (_, Or(ss)) => ss.foldLeft(a)(minus)
      case (a @ Typ(tp1, _), b @ Typ(tp2, _)) =>
        if isSubType(tp1, tp2) then Empty
        else if canDecompose(a) then minus(Or(decompose(a)), b)
        else if canDecompose(b) then minus(a, Or(decompose(b)))
        else a
      case (a @ Typ(tp1, _), Prod(tp2, fun, ss)) =>
        // rationale: every instance of `tp1` is covered by `tp2(_)`
        if isSubType(tp1, tp2) && covers(fun, tp1, ss.length) then
          minus(Prod(tp1, fun, signature(fun, tp1, ss.length).map(Typ(_, false))), b)
        else if canDecompose(a) then minus(Or(decompose(a)), b)
        else a
      case (Prod(tp1, fun, ss), b @ Typ(tp2, _)) =>
        // uncovered corner case: tp2 :< tp1, may happen when inheriting case class
        if isSubType(tp1, tp2) then Empty
        else if simplify(a) == Empty then Empty
        else if canDecompose(b) then minus(a, Or(decompose(b)))
        else a
      case (Prod(tp1, fun1, ss1), Prod(tp2, fun2, ss2))
          if !isSameUnapply(fun1, fun2) => a
      case (Prod(tp1, fun1, ss1), Prod(tp2, fun2, ss2))
          if fun1.symbol.name == nme.unapply && ss1.length != ss2.length => a
      case (a @ Prod(tp1, fun1, ss1), Prod(tp2, fun2, ss2)) =>
        val range = ss1.indices.toList
        val cache = Array.fill[Space | Null](ss2.length)(null)
        def sub(i: Int) =
          if cache(i) == null then
            cache(i) = minus(ss1(i), ss2(i))
          cache(i).nn

        if range.exists(i => isSubspace(ss1(i), sub(i))) then a
        else if cache.forall(sub => isSubspace(sub.nn, Empty)) then Empty
        else
          // `(_, _, _) - (Some, None, _)` becomes `(None, _, _) | (_, Some, _) | (_, _, Empty)`
          val spaces = LazyList(range*).flatMap { i =>
            flatten(sub(i)).map(s => Prod(tp1, fun1, ss1.updated(i, s)))
          }
          Or(spaces)
    }
  }

  /** Is the unapply or unapplySeq irrefutable?
   *  @param  unapp   The unapply function reference
   */
  def isIrrefutable(unapp: TermRef, argLen: Int)(using Context): Boolean = {
    val unappResult = unapp.widen.finalResultType
    unappResult.isRef(defn.SomeClass)
    || unappResult <:< ConstantType(Constant(true)) // only for unapply
    || (unapp.symbol.is(Synthetic) && unapp.symbol.owner.linkedClass.is(Case))  // scala2 compatibility
    || unapplySeqTypeElemTp(unappResult).exists // only for unapplySeq
    || isProductMatch(unappResult, argLen)
    || extractorMemberType(unappResult, nme.isEmpty, NoSourcePosition) <:< ConstantType(Constant(false))
    || unappResult.derivesFrom(defn.NonEmptyTupleClass)
    || unapp.symbol == defn.TupleXXL_unapplySeq // Fixes TupleXXL.unapplySeq which returns Some but declares Option
  }

  /** Is the unapply or unapplySeq irrefutable?
   *  @param  unapp   The unapply function tree
   */
  def isIrrefutable(unapp: tpd.Tree, argLen: Int)(using Context): Boolean = {
    tpd.funPart(unapp).tpe match
      case funRef: TermRef => isIrrefutable(funRef, argLen)
      case _: ErrorType => false
  }

  /** Is this an `'{..}` or `'[..]` irrefutable quoted patterns?
   *  @param  body The body of the quoted pattern
   *  @param  bodyPt The scrutinee body type
   */
  def isIrrefutableQuotePattern(pat: tpd.QuotePattern, pt: Type)(using Context): Boolean = {
    if pat.body.isType then pat.bindings.isEmpty && pt =:= pat.tpe
    else pat.body match
      case _: SplicePattern | Typed(_: SplicePattern, _) => pat.bindings.isEmpty && pt <:< pat.tpe
      case _ => false
  }

  /** Return a space containing the values of both types.
   *
   * The types should be atomic (non-decomposable) and unrelated (neither
   * should be a subtype of the other).
   */
  def intersectUnrelatedAtomicTypes(tp1: Type, tp2: Type)(sp: Space)(using Context): Space = trace(i"atomic intersection: ${AndType(tp1, tp2)}", debug, show) {
    // Precondition: !isSubType(tp1, tp2) && !isSubType(tp2, tp1).
    if !ctx.mode.is(Mode.SafeNulls) && (tp1.isNullType || tp2.isNullType) then
      // Since projections of types don't include null, intersection with null is empty.
      Empty
    else
      val intersection = sp match
        case sp: Prod => sp.copy(AndType(tp1, tp2))
        case _        => Typ(AndType(tp1, tp2), decomposed = false)
      // unrelated numeric value classes can equal each other, so let's not consider type space intersection empty
      if tp1.classSymbol.isNumericValueClass && tp2.classSymbol.isNumericValueClass then intersection
      else if isPrimToBox(tp1, tp2) || isPrimToBox(tp2, tp1) then intersection
      else if TypeComparer.provablyDisjoint(tp1, tp2) then Empty
      else intersection
  }

  /** Return the space that represents the pattern `pat` */
  def project(pat: Tree)(using Context): Space = trace(i"project($pat ${pat.className} ${pat.tpe})", debug, show)(pat match {
    case Literal(c) =>
      if (c.value.isInstanceOf[Symbol])
        Typ(c.value.asInstanceOf[Symbol].termRef, decomposed = false)
      else
        Typ(ConstantType(c), decomposed = false)

    case pat: Ident if isBackquoted(pat) =>
      Typ(pat.tpe, decomposed = false)

    case Ident(_) | Select(_, _) =>
      Typ(erase(pat.tpe.stripAnnots.widenSkolem, isValue = true), decomposed = false)

    case Alternative(trees) =>
      Or(trees.map(project(_)))

    case Bind(_, pat) =>
      project(pat)

    case SeqLiteral(pats, _) =>
      projectSeq(pats)

    case UnApply(fun, _, pats) =>
      val fun1 = funPart(fun)
      val funRef = fun1.tpe.asInstanceOf[TermRef]
      if (fun.symbol.name == nme.unapplySeq)
        val (arity, elemTp, resultTp) = unapplySeqInfo(fun.tpe.widen.finalResultType, fun.srcPos)
        if (fun.symbol.owner == defn.SeqFactoryClass && defn.ListType.appliedTo(elemTp) <:< pat.tpe)
          // The exhaustivity and reachability logic already handles decomposing sum types (into its subclasses)
          // and product types (into its components).  To get better counter-examples for patterns that are of type
          // List (or a super-type of list, like LinearSeq) we project them into spaces that use `::` and Nil.
          // Doing so with a pattern of `case Seq() =>` with a scrutinee of type `Vector()` doesn't work because the
          // space is then discarded leading to a false positive reachability warning, see #13931.
          projectSeq(pats)
        else {
          if (elemTp.exists)
            Prod(erase(pat.tpe.stripAnnots, isValue = false), funRef, projectSeq(pats) :: Nil)
          else
            Prod(erase(pat.tpe.stripAnnots, isValue = false), funRef, pats.take(arity - 1).map(project) :+ projectSeq(pats.drop(arity - 1)))
        }
      else
        Prod(erase(pat.tpe.stripAnnots, isValue = false), funRef, pats.map(project))

    case Typed(pat @ UnApply(_, _, _), _) =>
      project(pat)

    case Typed(_, tpt) =>
      Typ(erase(tpt.tpe.stripAnnots, isValue = true, isTyped = true), decomposed = false)

    case This(_) =>
      Typ(pat.tpe.stripAnnots, decomposed = false)

    case EmptyTree =>         // default rethrow clause of try/catch, check tests/patmat/try2.scala
      Typ(WildcardType, decomposed = false)

    case Block(Nil, expr) =>
      project(expr)

    case _ =>
      // Pattern is an arbitrary expression; assume a skolem (i.e. an unknown value) of the pattern type
      Typ(pat.tpe.narrow, decomposed = false)
  })

  private def project(tp: Type)(using Context): Space = tp match {
    case OrType(tp1, tp2) => Or(project(tp1) :: project(tp2) :: Nil)
    case tp => Typ(tp, decomposed = true)
  }

  private def unapplySeqInfo(resTp: Type, pos: SrcPos)(using Context): (Int, Type, Type) = {
    var resultTp = resTp
    var elemTp = unapplySeqTypeElemTp(resultTp)
    var arity = productArity(resultTp, pos)
    if (!elemTp.exists && arity <= 0) {
      resultTp = resTp.select(nme.get).finalResultType
      elemTp = unapplySeqTypeElemTp(resultTp.widen)
      arity = productSelectorTypes(resultTp, pos).size
    }
    (arity, elemTp, resultTp)
  }

  /** Erase pattern bound types with WildcardType
   *
   *  For example, the type `C[T$1]` should match any `C[?]`, thus
   *  `v` should be `WildcardType` instead of `T$1`:
   *
   *     sealed trait B
   *     case class C[T](v: T) extends B
   *     (b: B) match {
   *        case C(v) =>      //    case C.unapply[T$1 @ T$1](v @ _):C[T$1]
   *     }
   *
   *  However, we cannot use WildcardType for Array[?], due to that
   *  `Array[WildcardType] <: Array[Array[WildcardType]]`, which may
   *  cause false unreachable warnings. See tests/patmat/t2425.scala
   *
   *  We cannot use type erasure here, as it would lose the constraints
   *  involving GADTs. For example, in the following code, type
   *  erasure would lose the constraint that `x` and `y` must be
   *  the same type, resulting in false inexhaustive warnings:
   *
   *     sealed trait Expr[T]
   *     case class IntExpr(x: Int) extends Expr[Int]
   *     case class BooleanExpr(b: Boolean) extends Expr[Boolean]
   *
   *     def foo[T](x: Expr[T], y: Expr[T]) = (x, y) match {
   *       case (IntExpr(_), IntExpr(_)) =>
   *       case (BooleanExpr(_), BooleanExpr(_)) =>
   *     }
   *
   *  @param inArray whether `tp` is a type argument to `Array`
   *  @param isValue whether `tp` is the type which match against values
   *  @param isTyped whether `tp` is the type from a `Typed` tree
   *
   *  If `isValue` is true, then pattern-bound symbols are erased to its upper bound.
   *  This is needed to avoid spurious unreachable warnings. See tests/patmat/i6197.scala.
   */
  private def erase(tp: Type, inArray: Boolean = false, isValue: Boolean = false, isTyped: Boolean = false)(using Context): Type =
    trace(i"erase($tp${if inArray then " inArray" else ""}${if isValue then " isValue" else ""}${if isTyped then " isTyped" else ""})", debug)(tp match {
      case tp @ AppliedType(tycon, args) if tycon.typeSymbol.isPatternBound =>
        WildcardType

      case tp @ AppliedType(tycon, args) =>
        val inArray = tycon.isRef(defn.ArrayClass) || tp.translucentSuperType.isRef(defn.ArrayClass)
        val args2 =
          if isTyped && !inArray then args.map(_ => WildcardType)
          else args.map(arg => erase(arg, inArray = inArray, isValue = false))
        tp.derivedAppliedType(erase(tycon, inArray, isValue = false), args2)

      case tp @ OrType(tp1, tp2) =>
        OrType(erase(tp1, inArray, isValue, isTyped), erase(tp2, inArray, isValue, isTyped), tp.isSoft)

      case AndType(tp1, tp2) =>
        AndType(erase(tp1, inArray, isValue, isTyped), erase(tp2, inArray, isValue, isTyped))

      case tp @ RefinedType(parent, _, _) =>
        erase(parent, inArray, isValue, isTyped)

      case tref: TypeRef if tref.symbol.isPatternBound =>
        if inArray then erase(tref.underlying, inArray, isValue, isTyped)
        else if isValue then erase(tref.superType, inArray, isValue, isTyped)
        else WildcardType

      case _ => tp
    })

  /** Space of the pattern: unapplySeq(a, b, c*)
   */
  def projectSeq(pats: List[Tree])(using Context): Space = {
    if (pats.isEmpty) return Typ(defn.NilType, false)

    val (items, zero) = if (isWildcardStarArg(pats.last))
      (pats.init, Typ(defn.ListType.appliedTo(pats.last.tpe.elemType), false))
    else
      (pats, Typ(defn.NilType, false))

    val unapplyTp = defn.ConsType.classSymbol.companionModule.termRef.select(nme.unapply)
    items.foldRight[Space](zero) { (pat, acc) =>
      val consTp = defn.ConsType.appliedTo(pats.head.tpe.widen)
      Prod(consTp, unapplyTp, project(pat) :: acc :: Nil)
    }
  }

  def isPrimToBox(tp: Type, pt: Type)(using Context): Boolean =
    tp.isPrimitiveValueType && (defn.boxedType(tp).classSymbol eq pt.classSymbol)

  /** Is `tp1` a subtype of `tp2`?  */
  def isSubType(tp1: Type, tp2: Type)(using Context): Boolean = trace(i"$tp1 <:< $tp2", debug, show = true) {
    if tp1 == ConstantType(Constant(null)) && !ctx.mode.is(Mode.SafeNulls)
    then tp2 == ConstantType(Constant(null))
    else if tp1.isTupleXXLExtract(tp2) then true // See isTupleXXLExtract, fixes TupleXXL parameter type
    else tp1 <:< tp2
  }

  /** True if we can assume that the two unapply methods are the same.
   *  That is, given the same parameter, they return the same result.
   *
   *  We assume that unapply methods are pure, but the same method may
   *  be called with different prefixes, thus behaving differently.
   */
  def isSameUnapply(tp1: TermRef, tp2: TermRef)(using Context): Boolean = trace(i"isSameUnapply($tp1, $tp2)") {
    def isStable(tp: TermRef) =
      !tp.symbol.is(ExtensionMethod) // The "prefix" of an extension method may be, but the receiver isn't, so exclude
      && tp.prefix.isStable
    // always assume two TypeTest[S, T].unapply are the same if they are equal in types
    (isStable(tp1) && isStable(tp2) || tp1.symbol == defn.TypeTest_unapply)
    && tp1 =:= tp2
  }

  /** Return term parameter types of the extractor `unapp`.
   *  Parameter types of the case class type `tp`. Adapted from `unapplyPlan` in patternMatcher  */
  def signature(unapp: TermRef, scrutineeTp: Type, argLen: Int)(using Context): List[Type] = {
    val unappSym = unapp.symbol

    // println("scrutineeTp = " + scrutineeTp.show)

    val mt: MethodType = unapp.widen match {
      case mt: MethodType => mt
      case pt: PolyType   =>
          val tvars = constrained(pt)
          val mt = pt.instantiate(tvars).asInstanceOf[MethodType]
          scrutineeTp <:< mt.paramInfos(0)
          // force type inference to infer a narrower type: could be singleton
          // see tests/patmat/i4227.scala
          mt.paramInfos(0) <:< scrutineeTp
          instantiateSelected(mt, tvars)
          isFullyDefined(mt, ForceDegree.all)
          mt
    }

    // Case unapply:
    // 1. return types of constructor fields if the extractor is synthesized for Scala2 case classes & length match
    // 2. return Nil if unapply returns Boolean  (boolean pattern)
    // 3. return product selector types if unapply returns a product type (product pattern)
    // 4. return product selectors of `T` where `def get: T` is a member of the return type of unapply & length match (named-based pattern)
    // 5. otherwise, return `T` where `def get: T` is a member of the return type of unapply
    //
    // Case unapplySeq:
    // 1. return the type `List[T]` where `T` is the element type of the unapplySeq return type `Seq[T]`

    val resTp = ctx.typeAssigner.safeSubstMethodParams(mt, scrutineeTp :: Nil).finalResultType

    val sig =
      if (resTp.isRef(defn.BooleanClass))
        List()
      else {
        val isUnapplySeq = unappSym.name == nme.unapplySeq

        if (isUnapplySeq) {
          val (arity, elemTp, resultTp) = unapplySeqInfo(resTp, unappSym.srcPos)
          if (elemTp.exists) defn.ListType.appliedTo(elemTp) :: Nil
          else {
            val sels = productSeqSelectors(resultTp, arity, unappSym.srcPos)
            sels.init :+ defn.ListType.appliedTo(sels.last)
          }
        }
        else {
          val arity = productArity(resTp, unappSym.srcPos)
          if (arity > 0)
            productSelectorTypes(resTp, unappSym.srcPos)
          else {
            val getTp = resTp.select(nme.get).finalResultType match
              case tp: TermRef if !tp.isOverloaded =>
                // Like widenTermRefExpr, except not recursively.
                // For example, in i17184 widen Option[foo.type]#get
                // to Option[foo.type] instead of Option[Int].
                tp.underlying.widenExpr
              case tp => tp
            if (argLen == 1) getTp :: Nil
            else productSelectorTypes(getTp, unappSym.srcPos)
          }
        }
      }

    debug.println(s"signature of ${unappSym.showFullName} ----> ${sig.map(_.show).mkString(", ")}")

    sig.map(_.annotatedToRepeated)
  }

  /** Whether the extractor covers the given type */
  def covers(unapp: TermRef, scrutineeTp: Type, argLen: Int)(using Context): Boolean = trace(i"covers($unapp, $scrutineeTp, $argLen)") {
    SpaceEngine.isIrrefutable(unapp, argLen)
    || unapp.symbol == defn.TypeTest_unapply && {
      val AppliedType(_, _ :: tp :: Nil) = unapp.prefix.widen.dealias: @unchecked
      scrutineeTp <:< tp
    }
    || unapp.symbol == defn.ClassTagClass_unapply && {
      val AppliedType(_, tp :: Nil) = unapp.prefix.widen.dealias: @unchecked
      scrutineeTp <:< tp
    }
  }

  /** Decompose a type into subspaces -- assume the type can be decomposed */
  def decompose(tp: Type)(using Context): List[Type] = trace(i"decompose($tp)", debug) {
    def rec(tp: Type, mixins: List[Type]): List[Type] = tp.dealias match
      case AndType(tp1, tp2) =>
        var tpB   = tp2
        var parts = rec(tp1, tp2 :: mixins)
        if parts == ListOfNoType then
          tpB   = tp1
          parts = rec(tp2, tp1 :: mixins)
        if parts == ListOfNoType then ListOfNoType
        else parts.collect:
          case tp if tp <:< tpB                              => tp
          case tp if tpB <:< tp                              => tpB
          case tp if !TypeComparer.provablyDisjoint(tp, tpB) => AndType(tp, tpB)

      case OrType(tp1, tp2)                            => List(tp1, tp2)
      case tp if tp.isRef(defn.BooleanClass)           => List(ConstantType(Constant(true)), ConstantType(Constant(false)))
      case tp if tp.isRef(defn.UnitClass)              => ConstantType(Constant(())) :: Nil
      case tp @ NamedType(Parts(parts), _)             => parts.map(tp.derivedSelect)
      case _: SingletonType                            => ListOfNoType
      case tp if tp.classSymbol.isAllOf(JavaEnum)      => tp.classSymbol.children.map(_.termRef)
        // the class of a java enum value is the enum class, so this must follow SingletonType to not loop infinitely

      case tp @ AppliedType(Parts(parts), targs) if tp.classSymbol.children.isEmpty =>
        // It might not obvious that it's OK to apply the type arguments of a parent type to child types.
        // But this is guarded by `tp.classSymbol.children.isEmpty`,
        // meaning we'll decompose to the same class, just not the same type.
        // For instance, from i15029, `decompose((X | Y).Field[T]) = [X.Field[T], Y.Field[T]]`.
        parts.map(tp.derivedAppliedType(_, targs))

      case tp if tp.isDecomposableToChildren =>
        def getChildren(sym: Symbol): List[Symbol] =
          sym.children.flatMap { child =>
            if child eq sym then List(sym) // i3145: sealed trait Baz, val x = new Baz {}, Baz.children returns Baz...
            else if tp.classSymbol == defn.TupleClass || tp.classSymbol == defn.NonEmptyTupleClass then
              List(child) // TupleN and TupleXXL classes are used for Tuple, but they aren't Tuple's children
            else if (child.is(Private) || child.is(Sealed)) && child.isOneOf(AbstractOrTrait) then getChildren(child)
            else List(child)
          }
        val children = getChildren(tp.classSymbol)
        debug.println(i"candidates for $tp : $children")

        val parts = children.map { sym =>
          val sym1 = if (sym.is(ModuleClass)) sym.sourceModule else sym
          val refined = TypeOps.refineUsingParent(tp, sym1, mixins)
          debug.println(i"$sym1 refined to $refined")

          def inhabited(tp: Type): Boolean = tp.dealias match
            case AndType(tp1, tp2) => !TypeComparer.provablyDisjoint(tp1, tp2)
            case OrType(tp1, tp2) => inhabited(tp1) || inhabited(tp2)
            case tp: RefinedType => inhabited(tp.parent)
            case tp: TypeRef => inhabited(tp.prefix)
            case _ => true

          if inhabited(refined) then refined
          else NoType
        }.filter(_.exists)
        debug.println(i"$tp decomposes to $parts")
        parts

      case _ => ListOfNoType
    end rec

    rec(tp, Nil)
  }

  extension (tp: Type)
    /** A type is decomposable to children if it has a simple kind, it's sealed,
      * abstract (or a trait) - so its not a sealed concrete class that can be instantiated on its own,
      * has no anonymous children, which we wouldn't be able to name as counter-examples,
      * but does have children.
      *
      * A sealed trait with no subclasses is considered not decomposable and thus is treated as an opaque type.
      * A sealed trait with subclasses that then get removed after `refineUsingParent`, decomposes to the empty list.
      * So that's why we consider whether a type has children. */
    def isDecomposableToChildren(using Context): Boolean =
      val cls = tp.classSymbol
      tp.hasSimpleKind && cls.is(Sealed) && cls.isOneOf(AbstractOrTrait) && !cls.hasAnonymousChild && cls.children.nonEmpty

  val ListOfNoType    = List(NoType)
  val ListOfTypNoType = ListOfNoType.map(Typ(_, decomposed = true))

  object Parts:
    def unapply(tp: Type)(using Context): PartsExtractor = PartsExtractor(decompose(tp))

  final class PartsExtractor(val get: List[Type]) extends AnyVal:
    def isEmpty: Boolean = get == ListOfNoType

  /** Show friendly type name with current scope in mind
   *
   *  E.g.    C.this.B     -->  B     if current owner is C
   *          C.this.x.T   -->  x.T   if current owner is C
   *          X[T]         -->  X
   *          C            -->  C     if current owner is C !!!
   *
   */
  def showType(tp: Type, showTypeArgs: Boolean = false)(using Context): String = {
    val enclosingCls = ctx.owner.enclosingClass

    def isOmittable(sym: Symbol) =
      sym.isEffectiveRoot || sym.isAnonymousClass || sym.name.isReplWrapperName ||
        ctx.definitions.unqualifiedOwnerTypes.exists(_.symbol == sym) ||
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

    def refine(tp: Type): String = tp.stripped match {
      case tp: RefinedType => refine(tp.parent)
      case tp: AppliedType =>
        refine(tp.typeConstructor) + (
          if (showTypeArgs) tp.argInfos.map(refine).mkString("[", ",", "]")
          else ""
        )
      case tp: ThisType => refine(tp.tref)
      case tp: NamedType =>
        val pre = refinePrefix(tp.prefix)
        if (tp.name == tpnme.higherKinds) pre
        else if (pre.isEmpty) tp.name.show.stripSuffix("$")
        else pre + "." + tp.name.show.stripSuffix("$")
      case tp: OrType => refine(tp.tp1) + " | " + refine(tp.tp2)
      case _: TypeBounds => "_"
      case _ => tp.show.stripSuffix("$")
    }

    refine(tp)
  }

  /** Whether the counterexample is satisfiable. The space is flattened and non-empty. */
  def satisfiable(sp: Space)(using Context): Boolean = {
    def impossible: Nothing = throw new AssertionError("`satisfiable` only accepts flattened space.")

    def genConstraint(space: Space): List[(Type, Type)] = space match {
      case Prod(tp, unappTp, ss) =>
        val tps = signature(unappTp, tp, ss.length)
        ss.zip(tps).flatMap {
          case (sp : Prod, tp) => sp.tp -> tp :: genConstraint(sp)
          case (Typ(tp1, _), tp2) => tp1 -> tp2 :: Nil
          case _ => impossible
        }
      case Typ(_, _) => Nil
      case _ => impossible
    }

    def checkConstraint(constrs: List[(Type, Type)])(using Context): Boolean = {
      val tvarMap = collection.mutable.Map.empty[Symbol, TypeVar]
      val typeParamMap = new TypeMap() {
        override def apply(tp: Type): Type = tp match {
          case tref: TypeRef if tref.symbol.is(TypeParam) =>
            tvarMap.getOrElseUpdate(tref.symbol, newTypeVar(tref.underlying.bounds))
          case tp => mapOver(tp)
        }
      }

      constrs.forall { case (tp1, tp2) => typeParamMap(tp1) <:< typeParamMap(tp2) }
    }

    checkConstraint(genConstraint(sp))(using ctx.fresh.setNewTyperState())
  }

  def showSpaces(ss: Seq[Space])(using Context): Seq[String] = ss.map(show)

  /** Display spaces */
  def show(s: Space)(using Context): String = {
    def params(tp: Type): List[Type] = tp.classSymbol.primaryConstructor.info.firstParamTypes

    /** does the companion object of the given symbol have custom unapply */
    def hasCustomUnapply(sym: Symbol): Boolean = {
      val companion = sym.companionModule
      companion.findMember(nme.unapply, NoPrefix, required = EmptyFlags, excluded = Synthetic).exists ||
        companion.findMember(nme.unapplySeq, NoPrefix, required = EmptyFlags, excluded = Synthetic).exists
    }

    def doShow(s: Space, flattenList: Boolean = false): String = s match {
      case Empty => "empty"
      case Typ(c: ConstantType, _) => c.value.show
      case Typ(tp: TermRef, _) =>
        if (flattenList && tp <:< defn.NilType) ""
        else tp.symbol.showName
      case Typ(tp, decomposed) =>

        val sym = tp.classSymbol

        if (ctx.definitions.isTupleNType(tp))
          params(tp).map(_ => "_").mkString("(", ", ", ")")
        else if (defn.ListType.isRef(sym))
          if (flattenList) "_*" else "_: List"
        else if (defn.ConsType.isRef(sym))
          if (flattenList) "_, _*"  else "List(_, _*)"
        else if (tp.classSymbol.is(Sealed) && tp.classSymbol.hasAnonymousChild)
          "_: " + showType(tp) + " (anonymous)"
        else if (tp.classSymbol.is(CaseClass) && !hasCustomUnapply(tp.classSymbol))
        // use constructor syntax for case class
          showType(tp) + params(tp).map(_ => "_").mkString("(", ", ", ")")
        else if (decomposed) "_: " + showType(tp, showTypeArgs = true)
        else "_"
      case Prod(tp, fun, params) =>
        if (ctx.definitions.isTupleNType(tp))
          "(" + params.map(doShow(_)).mkString(", ") + ")"
        else if (tp.isRef(defn.ConsType.symbol))
          if (flattenList) params.map(doShow(_, flattenList)).filter(_.nonEmpty).mkString(", ")
          else params.map(doShow(_, flattenList = true)).filter(!_.isEmpty).mkString("List(", ", ", ")")
        else {
          val sym = fun.symbol
          val isUnapplySeq = sym.name.eq(nme.unapplySeq)
          val paramsStr = params.map(doShow(_, flattenList = isUnapplySeq)).mkString("(", ", ", ")")
          showType(fun.prefix) + paramsStr
        }
      case Or(ss) =>
        ss.map(doShow(_, flattenList)).mkString(" | ")
    }

    doShow(s, flattenList = false)
  }

  private def exhaustivityCheckable(sel: Tree)(using Context): Boolean = {
    val seen = collection.mutable.Set.empty[Type]

    // Possible to check everything, but be compatible with scalac by default
    def isCheckable(tp: Type): Boolean =
      val tpw = tp.widen.dealias
      val classSym = tpw.classSymbol
      classSym.is(Sealed) && !tpw.isLargeGenericTuple || // exclude large generic tuples from exhaustivity
                                                         // requires an unknown number of changes to make work
      tpw.isInstanceOf[OrType] ||
      (tpw.isInstanceOf[AndType] && {
        val and = tpw.asInstanceOf[AndType]
        isCheckable(and.tp1) || isCheckable(and.tp2)
      }) ||
      tpw.isRef(defn.BooleanClass) ||
      classSym.isAllOf(JavaEnum) ||
      classSym.is(Case) && {
        if seen.add(tpw) then productSelectorTypes(tpw, sel.srcPos).exists(isCheckable(_))
        else true // recursive case class: return true and other members can still fail the check
      }

    val res = !sel.tpe.hasAnnotation(defn.UncheckedAnnot) && {
      ctx.settings.YcheckAllPatmat.value
      || isCheckable(sel.tpe)
    }

    debug.println(s"exhaustivity checkable: ${sel.show} = $res")
    res
  }

  /** Whether counter-examples should be further checked? True for GADTs. */
  private def shouldCheckExamples(tp: Type)(using Context): Boolean =
    new TypeAccumulator[Boolean] {
      override def apply(b: Boolean, tp: Type): Boolean = tp match {
        case tref: TypeRef if tref.symbol.is(TypeParam) && variance != 1 => true
        case tp => b || foldOver(b, tp)
      }
    }.apply(false, tp)

  /** Return the underlying type of non-module, non-constant, non-enum case singleton types.
   *  Also widen ExprType to its result type, and rewrap any annotation wrappers.
   *  For example, with `val opt = None`, widen `opt.type` to `None.type`. */
  def toUnderlying(tp: Type)(using Context): Type = trace(i"toUnderlying($tp)", show = true)(tp match {
    case _: ConstantType                            => tp
    case tp: TermRef if tp.symbol.is(Module)        => tp
    case tp: TermRef if tp.symbol.isAllOf(EnumCase) => tp
    case tp: SingletonType                          => toUnderlying(tp.underlying)
    case tp: ExprType                               => toUnderlying(tp.resultType)
    case AnnotatedType(tp, annot)                   => AnnotatedType(toUnderlying(tp), annot)
    case _                                          => tp
  })

  def checkExhaustivity(m: Match)(using Context): Unit = if exhaustivityCheckable(m.selector) then trace(i"checkExhaustivity($m)", debug) {
    val selTyp = toUnderlying(m.selector.tpe).dealias
    debug.println(i"selTyp = $selTyp")

    val patternSpace = Or(m.cases.foldLeft(List.empty[Space]) { (acc, x) =>
      val space = if (x.guard.isEmpty) project(x.pat) else Empty
      debug.println(s"${x.pat.show} ====> ${show(space)}")
      space :: acc
    })

    val checkGADTSAT = shouldCheckExamples(selTyp)

    val uncovered =
      flatten(simplify(minus(project(selTyp), patternSpace))).filter({ s =>
        s != Empty && (!checkGADTSAT || satisfiable(s))
      })


    if uncovered.nonEmpty then
      val deduped = dedup(uncovered)
      report.warning(PatternMatchExhaustivity(showSpaces(deduped), m), m.selector)
  }

  private def redundancyCheckable(sel: Tree)(using Context): Boolean =
    // Ignore Expr[T] and Type[T] for unreachability as a special case.
    // Quote patterns produce repeated calls to the same unapply method, but with different implicit parameters.
    // Since we assume that repeated calls to the same unapply method overlap
    // and implicit parameters cannot normally differ between two patterns in one `match`,
    // the easiest solution is just to ignore Expr[T] and Type[T].
    !sel.tpe.hasAnnotation(defn.UncheckedAnnot)
    && !sel.tpe.widen.isRef(defn.QuotedExprClass)
    && !sel.tpe.widen.isRef(defn.QuotedTypeClass)

  def checkRedundancy(m: Match)(using Context): Unit = if redundancyCheckable(m.selector) then trace(i"checkRedundancy($m)", debug) {
    val cases = m.cases.toIndexedSeq

    val selTyp = toUnderlying(m.selector.tpe).dealias
    debug.println(i"selTyp = $selTyp")

    val isNullable = selTyp.classSymbol.isNullableClass
    val targetSpace = if isNullable
      then project(OrType(selTyp, ConstantType(Constant(null)), soft = false))
      else project(selTyp)
    debug.println(s"targetSpace: ${show(targetSpace)}")

    var i        = 0
    val len      = cases.length
    var prevs    = List.empty[Space]
    var deferred = List.empty[Tree]

    while (i < len) {
      val CaseDef(pat, guard, _) = cases(i)

      debug.println(i"case pattern: $pat")

      val curr = project(pat)
      debug.println(i"reachable? ${show(curr)}")

      val prev = simplify(Or(prevs))
      debug.println(s"prev: ${show(prev)}")

      val covered = simplify(intersect(curr, targetSpace))
      debug.println(s"covered: ${show(covered)}")

      if prev == Empty && covered == Empty then // defer until a case is reachable
        deferred ::= pat
      else {
        for (pat <- deferred.reverseIterator)
          report.warning(MatchCaseUnreachable(), pat.srcPos)
        if pat != EmptyTree // rethrow case of catch uses EmptyTree
            && !pat.symbol.isAllOf(SyntheticCase, butNot=Method) // ExpandSAMs default cases use SyntheticCase
            && isSubspace(covered, prev)
        then {
          val nullOnly = isNullable && i == len - 1 && isWildcardArg(pat)
          val msg = if nullOnly then MatchCaseOnlyNullWarning() else MatchCaseUnreachable()
          report.warning(msg, pat.srcPos)
        }
        deferred = Nil
      }

      // in redundancy check, take guard as false in order to soundly approximate
      prevs ::= (if guard.isEmpty then covered else Empty)
      i += 1
    }
  }
}
