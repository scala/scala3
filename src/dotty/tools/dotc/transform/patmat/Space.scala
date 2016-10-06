package dotty.tools.dotc
package transform
package patmat

import core.Types._
import core.Contexts._
import core.Flags._
import ast.Trees._
import ast.tpd
import core.Decorators._
import core.Symbols._
import core.StdNames._
import core.NameOps._
import core.Constants._

/** Space logic for checking exhaustivity and unreachability of pattern matching
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
 *      4. For a case class Kon(x1: T1, x2: T2, .., xn: Tn), if S1, S2, ..., Sn
 *          are spaces, then `Kon(S1, S2, ..., Sn)` is a space.
 *      5. A constant `Const(value, T)` is a point in space
 *      6. A stable identifier `Var(sym, T)` is a space
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
sealed trait Space

/** Empty space */
case object Empty extends Space

/** Space representing the set of all values of a type
 *
 * @param tp: the type this space represents
 * @param decomposed: does the space result from decomposition? Used for pretty print
 *
 */
case class Typ(tp: Type, decomposed: Boolean) extends Space

/** Space representing a constructor pattern */
case class Kon(tp: Type, params: List[Space]) extends Space

/** Union of spaces */
case class Or(spaces: List[Space]) extends Space

/** Point in space */
sealed trait Point extends Space

/** Point representing variables(stable identifier) in patterns */
case class Var(sym: Symbol, tp: Type) extends Point

/** Point representing literal constants in patterns */
case class Const(value: Constant, tp: Type) extends Point

/** abstract space logic */
trait SpaceLogic {
  /** Is `tp1` a subtype of `tp2`? */
  def isSubType(tp1: Type, tp2: Type): Boolean

  /** Is `tp1` the same type as `tp2`? */
  def isEqualType(tp1: Type, tp2: Type): Boolean

  /** Is the type `tp` decomposable? i.e. all values of the type can be covered
   *  by its decomposed types.
   *
   * Abstract sealed class, OrType, Boolean and Java enums can be decomposed.
   */
  def canDecompose(tp: Type): Boolean

  /** Return term parameter types of the case class `tp` */
  def signature(tp: Type): List[Type]

  /** Get components of decomposable types */
  def decompose(tp: Type): List[Space]

  /** Simplify space using the laws, there's no nested union after simplify */
  def simplify(space: Space): Space = space match {
    case Kon(tp, spaces) =>
      val sp = Kon(tp, spaces.map(simplify _))
      if (sp.params.contains(Empty)) Empty
      else sp
    case Or(spaces) =>
      val set = spaces.map(simplify _).flatMap {
        case Or(ss) => ss
        case s => Seq(s)
      } filter (_ != Empty)

      if (set.isEmpty) Empty
      else if (set.size == 1) set.toList(0)
      else Or(set)
    case Typ(tp, _) =>
      if (canDecompose(tp) && decompose(tp).isEmpty) Empty
      else space
    case _ => space
  }

  /** Flatten space to get rid of `Or` for pretty print */
  def flatten(space: Space): List[Space] = space match {
    case Kon(tp, spaces) =>
      val flats = spaces.map(flatten _)

      flats.foldLeft(List[Kon]()) { (acc, flat) =>
        if (acc.isEmpty) flat.map(s => Kon(tp, Nil :+ s))
        else for (Kon(tp, ss) <- acc; s <- flat) yield Kon(tp, ss :+ s)
      }
    case Or(spaces) =>
      spaces.flatMap(flatten _)
    case _ => List(space)
  }

  /** Is `a` a subspace of `b`? Equivalent to `a - b == Empty`, but faster */
  def isSubspace(a: Space, b: Space): Boolean = {
    def tryDecompose1(tp: Type) = canDecompose(tp) && isSubspace(Or(decompose(tp)), b)
    def tryDecompose2(tp: Type) = canDecompose(tp) && isSubspace(a, Or(decompose(tp)))

    (a, b) match {
      case (Empty, _) => true
      case (_, Empty) => false
      case (Or(ss), _) => ss.forall(isSubspace(_, b))
      case (Typ(tp1, _), Typ(tp2, _)) =>
        isSubType(tp1, tp2) || tryDecompose1(tp1) || tryDecompose2(tp2)
      case (Typ(tp1, _), Or(ss)) =>
        ss.exists(isSubspace(a, _)) || tryDecompose1(tp1)
      case (Typ(tp1, _), Kon(tp2, ss)) =>
        isSubType(tp1, tp2) && isSubspace(Kon(tp2, signature(tp2).map(Typ(_, false))), b) ||
        tryDecompose1(tp1)
      case (Kon(tp1, ss), Typ(tp2, _)) =>
        isSubType(tp1, tp2) ||
          simplify(a) == Empty ||
          (isSubType(tp2, tp1) && tryDecompose1(tp1)) ||
          tryDecompose2(tp2)
      case (Kon(_, _), Or(_)) =>
        simplify(minus(a, b)) == Empty
      case (Kon(tp1, ss1), Kon(tp2, ss2)) =>
        isEqualType(tp1, tp2) && ss1.zip(ss2).forall((isSubspace _).tupled)
      case (Const(v1, _), Const(v2, _)) => v1 == v2
      case (Const(_, tp1), Typ(tp2, _)) => isSubType(tp1, tp2) || tryDecompose2(tp2)
      case (Const(_, _), Or(ss)) => ss.exists(isSubspace(a, _))
      case (Const(_, _), _) => false
      case (_, Const(_, _)) => false
      case (Var(x, _), Var(y, _)) => x == y
      case (Var(_, tp1), Typ(tp2, _)) => isSubType(tp1, tp2) || tryDecompose2(tp2)
      case (Var(_, _), Or(ss)) => ss.exists(isSubspace(a, _))
      case (Var(_, _), _) => false
      case (_, Var(_, _)) => false
    }
  }

  /** Intersection of two spaces  */
  def intersect(a: Space, b: Space): Space = {
    def tryDecompose1(tp: Type) = intersect(Or(decompose(tp)), b)
    def tryDecompose2(tp: Type) = intersect(a, Or(decompose(tp)))

    (a, b) match {
      case (Empty, _) | (_, Empty) => Empty
      case (_, Or(ss)) => Or(ss.map(intersect(a, _)).filterConserve(_ ne Empty))
      case (Or(ss), _) => Or(ss.map(intersect(_, b)).filterConserve(_ ne Empty))
      case (Typ(tp1, _), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) a
        else if (isSubType(tp2, tp1)) b
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else Empty
      case (Typ(tp1, _), Kon(tp2, ss)) =>
        if (isSubType(tp2, tp1)) b
        else if (isSubType(tp1, tp2)) a // problematic corner case: inheriting a case class
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else Empty
      case (Kon(tp1, ss), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) a
        else if (isSubType(tp2, tp1)) a  // problematic corner case: inheriting a case class
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else Empty
      case (Kon(tp1, ss1), Kon(tp2, ss2)) =>
        if (!isEqualType(tp1, tp2)) Empty
        else if (ss1.zip(ss2).exists(p => simplify(intersect(p._1, p._2)) == Empty)) Empty
        else Kon(tp1, ss1.zip(ss2).map((intersect _).tupled))
      case (Const(v1, _), Const(v2, _)) =>
        if (v1 == v2) a else Empty
      case (Const(_, tp1), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) a
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else Empty
      case (Const(_, _), _) => Empty
      case (Typ(tp1, _), Const(_, tp2)) =>
        if (isSubType(tp2, tp1)) b
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else Empty
      case (_, Const(_, _)) => Empty
      case (Var(x, _), Var(y, _)) =>
        if (x == y) a else Empty
      case (Var(_, tp1), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) a
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else Empty
      case (Var(_, _), _) => Empty
      case (Typ(tp1, _), Var(_, tp2)) =>
        if (isSubType(tp2, tp1)) b
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else Empty
      case (_, Var(_, _)) => Empty
    }
  }

  /** The space of a not covered by b */
  def minus(a: Space, b: Space): Space = {
    def tryDecompose1(tp: Type) = minus(Or(decompose(tp)), b)
    def tryDecompose2(tp: Type) = minus(a, Or(decompose(tp)))

    (a, b) match {
      case (Empty, _) => Empty
      case (_, Empty) => a
      case (Typ(tp1, _), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) Empty
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else a
      case (Typ(tp1, _), Kon(tp2, ss)) =>
        // corner case: inheriting a case class
        // rationale: every instance of `tp1` is covered by `tp2(_)`
        if (isSubType(tp1, tp2)) minus(Kon(tp2, signature(tp2).map(Typ(_, false))), b)
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else a
      case (_, Or(ss)) =>
        ss.foldLeft(a)(minus)
      case (Or(ss), _) =>
        Or(ss.map(minus(_, b)))
      case (Kon(tp1, ss), Typ(tp2, _)) =>
        // uncovered corner case: tp2 :< tp1
        if (isSubType(tp1, tp2)) Empty
        else if (simplify(a) == Empty) Empty
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else a
      case (Kon(tp1, ss1), Kon(tp2, ss2)) =>
        if (!isEqualType(tp1, tp2)) a
        else if (ss1.zip(ss2).exists(p => simplify(intersect(p._1, p._2)) == Empty)) a
        else if (ss1.zip(ss2).forall((isSubspace _).tupled)) Empty
        else
          // `(_, _, _) - (Some, None, _)` becomes `(None, _, _) | (_, Some, _) | (_, _, Empty)`
          Or(ss1.zip(ss2).map((minus _).tupled).zip(0 to ss2.length - 1).map {
              case (ri, i) => Kon(tp1, ss1.updated(i, ri))
            })
      case (Const(v1, _), Const(v2, _)) =>
        if (v1 == v2) Empty else a
      case (Const(_, tp1), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) Empty
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else a
      case (Const(_, _), _) => a
      case (Typ(tp1, _), Const(_, tp2)) =>  // Boolean & Java enum
        if (canDecompose(tp1)) tryDecompose1(tp1)
        else a
      case (_, Const(_, _)) => a
      case (Var(x, _), Var(y, _)) =>
        if (x == y) Empty else a
      case (Var(_, tp1), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) Empty
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else a
      case (Var(_, _), _) => a
      case (_, Var(_, _)) => a
    }
  }
}

/** Scala implementation of space logic */
class SpaceEngine(implicit ctx: Context) extends SpaceLogic {
  import tpd._

  /** Return the space that represents the pattern `pat`
   *
   *  If roundUp is true, approximate extractors to its type,
   *  otherwise approximate extractors to Empty
   */
  def project(pat: Tree, roundUp: Boolean = true)(implicit ctx: Context): Space = pat match {
    case Literal(c) => Const(c, c.tpe)
    case _: BackquotedIdent => Var(pat.symbol, pat.tpe)
    case Ident(_) | Select(_, _) =>
      pat.tpe.stripAnnots match {
        case tp: TermRef =>
          if (pat.symbol.is(Enum))
            Const(Constant(pat.symbol), tp)
          else if (tp.underlyingIterator.exists(_.classSymbol.is(Module)))
            Typ(tp.widenTermRefExpr.stripAnnots, false)
          else
            Var(pat.symbol, tp)
        case tp => Typ(tp, false)
      }
    case Alternative(trees) => Or(trees.map(project(_, roundUp)))
    case Bind(_, pat) => project(pat)
    case UnApply(_, _, pats) =>
      if (pat.tpe.classSymbol.is(CaseClass))
        Kon(pat.tpe.stripAnnots, pats.map(pat => project(pat, roundUp)))
      else if (roundUp) Typ(pat.tpe.stripAnnots, false)
      else Empty
    case Typed(pat @ UnApply(_, _, _), _) => project(pat)
    case Typed(expr, _) => Typ(expr.tpe.stripAnnots, true)
    case _ =>
      Empty
  }

  /* Erase a type binding according to erasure semantics in pattern matching */
  def erase(tp: Type): Type = {
    def doErase(tp: Type): Type = tp match {
      case tp: HKApply => erase(tp.superType)
      case tp: RefinedType => erase(tp.parent)
      case _ => tp
    }

    tp match {
      case OrType(tp1, tp2) =>
        OrType(erase(tp1), erase(tp2))
      case AndType(tp1, tp2) =>
        AndType(erase(tp1), erase(tp2))
      case _ =>
        val origin = doErase(tp)
        if (origin =:= defn.ArrayType) tp else origin
    }
  }

  /** Is `tp1` a subtype of `tp2`?  */
  def isSubType(tp1: Type, tp2: Type): Boolean = {
    // check SI-9657 and tests/patmat/gadt.scala
    erase(tp1) <:< erase(tp2)
  }

  def isEqualType(tp1: Type, tp2: Type): Boolean = tp1 =:= tp2

  /** Parameter types of the case class type `tp`  */
  def signature(tp: Type): List[Type] = {
    val ktor = tp.classSymbol.primaryConstructor.info

    val meth = ktor match {
      case ktor: PolyType =>
        ktor.instantiate(tp.classSymbol.typeParams.map(_.typeRef)).asSeenFrom(tp, tp.classSymbol)
      case _ => ktor
    }

    // refine path-dependent type in params. refer to t9672
    meth.firstParamTypes.map(_.asSeenFrom(tp, tp.classSymbol))
  }

  /** Decompose a type into subspaces -- assume the type can be decomposed */
  def decompose(tp: Type): List[Space] = {
    val children = tp.classSymbol.annotations.filter(_.symbol == ctx.definitions.ChildAnnot).map { annot =>
      // refer to definition of Annotation.makeChild
      annot.tree match {
        case Apply(TypeApply(_, List(tpTree)), _) => tpTree.symbol
      }
    }

    tp match {
      case OrType(tp1, tp2) => List(Typ(tp1, true), Typ(tp2, true))
      case _ if tp =:= ctx.definitions.BooleanType =>
        List(
          Const(Constant(true), ctx.definitions.BooleanType),
          Const(Constant(false), ctx.definitions.BooleanType)
        )
      case _ if tp.classSymbol.is(Enum) =>
        children.map(sym => Const(Constant(sym), tp))
      case _ =>
        val parts = children.map { sym =>
          if (sym.is(ModuleClass))
            sym.asClass.classInfo.selfType
          else if (sym.info.typeParams.length > 0 || tp.isInstanceOf[TypeRef])
            refine(tp, sym.typeRef)
          else
            sym.typeRef
        } filter { tpe =>
          // Child class may not always be subtype of parent:
          // GADT & path-dependent types
          tpe <:< expose(tp)
        }

        parts.map(Typ(_, true))
    }
  }

  /** Refine tp2 based on tp1
   *
   *  E.g. if `tp1` is `Option[Int]`, `tp2` is `Some`, then return
   *  `Some[Int]`.
   *
   *  If `tp1` is `path1.A`, `tp2` is `path2.B`, and `path1` is subtype of
   *  `path2`, then return `path1.B`.
   */
  def refine(tp1: Type, tp2: Type): Type = (tp1, tp2) match {
    case (tp1: RefinedType, _) => tp1.wrapIfMember(refine(tp1.parent, tp2))
    case (tp1: HKApply, _) => refine(tp1.superType, tp2)
    case (TypeRef(ref1: TypeProxy, _), tp2 @ TypeRef(ref2: TypeProxy, name)) =>
      if (ref1.underlying <:< ref2.underlying) TypeRef(ref1, name) else tp2
    case _ => tp2
  }

  /** Abstract sealed types, or-types, Boolean and Java enums can be decomposed */
  def canDecompose(tp: Type): Boolean = {
    tp.classSymbol.is(allOf(Abstract, Sealed)) ||
      tp.classSymbol.is(allOf(Trait, Sealed)) ||
      tp.isInstanceOf[OrType] ||
      tp =:= ctx.definitions.BooleanType ||
      tp.classSymbol.is(Enum)
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
    val enclosingCls = ctx.owner.enclosingClass.asClass.classInfo.symbolicTypeRef

    def isOmittable(sym: Symbol) =
      sym.isEffectiveRoot || sym.isAnonymousClass || sym.name.isReplWrapperName ||
        ctx.definitions.UnqualifiedOwnerTypes.exists(_.symbol == sym) ||
        sym.showFullName.startsWith("scala.") ||
        sym == enclosingCls.typeSymbol

    def refinePrefix(tp: Type): String = tp match {
      case NoPrefix => ""
      case tp: NamedType if isOmittable(tp.symbol) => ""
      case tp: ThisType => refinePrefix(tp.tref)
      case tp: RefinedType => refinePrefix(tp.parent)
      case tp: NamedType => tp.name.show.stripSuffix("$")
    }

    def refine(tp: Type): String = tp match {
      case tp: RefinedType => refine(tp.parent)
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
  def show(s: Space): String = {
    def doShow(s: Space, mergeList: Boolean = false): String = s match {
      case Empty => ""
      case Const(v, _) => v.show
      case Var(x, _) => x.show
      case Typ(tp, decomposed) =>
        val sym = tp.widen.classSymbol

        if (sym.is(ModuleClass))
          showType(tp)
        else if (ctx.definitions.isTupleType(tp))
          signature(tp).map(_ => "_").mkString("(", ", ", ")")
        else if (sym.showFullName == "scala.collection.immutable.::")
          if (mergeList) "_" else "List(_)"
        else if (tp.classSymbol.is(CaseClass))
        // use constructor syntax for case class
          showType(tp) + signature(tp).map(_ => "_").mkString("(", ", ", ")")
        else if (signature(tp).nonEmpty)
          tp.classSymbol.name + signature(tp).map(_ => "_").mkString("(", ", ", ")")
        else if (decomposed) "_: " + showType(tp)
        else "_"
      case Kon(tp, params) =>
        if (ctx.definitions.isTupleType(tp))
          "(" + params.map(doShow(_)).mkString(", ") + ")"
        else if (tp.widen.classSymbol.showFullName == "scala.collection.immutable.::")
          if (mergeList) params.map(doShow(_, mergeList)).mkString(", ")
          else params.map(doShow(_, true)).filter(_ != "Nil").mkString("List(", ", ", ")")
        else
          showType(tp) + params.map(doShow(_)).mkString("(", ", ", ")")
      case Or(_) =>
        throw new Exception("incorrect flatten result " + s)
    }

    flatten(s).map(doShow(_, false)).distinct.mkString(", ")
  }

  def checkable(tree: Match): Boolean = {
    def isCheckable(tp: Type): Boolean = tp match {
      case AnnotatedType(tp, annot) =>
        (ctx.definitions.UncheckedAnnot != annot.symbol) && isCheckable(tp)
      case _ =>
        // Possible to check everything, but be compatible with scalac by default
        ctx.settings.YcheckAllPatmat.value ||
          tp.typeSymbol.is(Sealed) ||
          tp.isInstanceOf[OrType] ||
          tp.typeSymbol == ctx.definitions.BooleanType.typeSymbol ||
          tp.typeSymbol.is(Enum) ||
          canDecompose(tp) ||
          (defn.isTupleType(tp) && tp.dealias.argInfos.exists(isCheckable(_)))
    }

    val Match(sel, cases) = tree
    isCheckable(sel.tpe.widen.deAnonymize.dealiasKeepAnnots)
  }


  /** Expose refined type to eliminate reference to type variables
   *
   *  A = B                      M { type T = A }        ~~>  M { type T = B }
   *
   *  A <: X :> Y                M { type T = A }        ~~>  M { type T <: X :> Y }
   *
   *  A <: X :> Y  B <: U :> V   M { type T <: A :> B }  ~~>  M { type T <: X :> V }
   *
   *  A = X  B = Y               M { type T <: A :> B }  ~~>  M { type T <: X :> Y }
   */
  def expose(tp: Type): Type = {
    def follow(tp: Type, up: Boolean): Type = tp match {
      case tp: TypeProxy =>
        tp.underlying match {
          case TypeBounds(lo, hi) =>
            follow(if (up) hi else lo, up)
          case _ =>
            tp
        }
      case OrType(tp1, tp2) =>
        OrType(follow(tp1, up), follow(tp2, up))
      case AndType(tp1, tp2) =>
        AndType(follow(tp1, up), follow(tp2, up))
    }

    tp match {
      case tp: RefinedType =>
        tp.refinedInfo match {
          case tpa : TypeAlias =>
            val hi = follow(tpa.alias, true)
            val lo = follow(tpa.alias, false)
            val refined = if (hi =:= lo)
              tpa.derivedTypeAlias(hi)
            else
              tpa.derivedTypeBounds(lo, hi)

            tp.derivedRefinedType(
              expose(tp.parent),
              tp.refinedName,
              refined
            )
          case tpb @ TypeBounds(lo, hi) =>
            tp.derivedRefinedType(
              expose(tp.parent),
              tp.refinedName,
              tpb.derivedTypeBounds(follow(lo, false), follow(hi, true))
            )
        }
      case _ => tp
    }
  }

  def checkExhaustivity(_match: Match): Unit = {
    val Match(sel, cases) = _match
    val selTyp = sel.tpe.widen.deAnonymize.dealias


    val patternSpace = cases.map(x => project(x.pat)).reduce((a, b) => Or(List(a, b)))
    val uncovered = simplify(minus(Typ(selTyp, true), patternSpace))

    if (uncovered != Empty) {
      ctx.warning(
        "match may not be exhaustive.\n" +
        s"It would fail on the following input: " +
        show(uncovered), _match.pos
      )
    }
  }

  def checkRedundancy(_match: Match): Unit = {
    val Match(sel, cases) = _match
    // ignore selector type for now
    // val selTyp = sel.tpe.widen.deAnonymize.dealias

    // starts from the second, the first can't be redundant
    (1 until cases.length).foreach { i =>
      // in redundancy check, take guard as false, take extractor as match
      // nothing in order to soundly approximate
      val prevs = cases.take(i).map { x =>
        if (x.guard.isEmpty) project(x.pat, false)
        else Empty
      }.reduce((a, b) => Or(List(a, b)))

      val curr = project(cases(i).pat)

      if (isSubspace(curr, prevs)) {
        ctx.warning("unreachable code", cases(i).body.pos)
      }
    }
  }
}
