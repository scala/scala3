package dotty.tools.dotc
package transform
package patmat

import core.Types._
import core.Contexts._
import core.Flags._
import ast.Trees._
import ast.tpd
import core.Decorators._
import core.TypeApplications._
import core.Symbols._
import core.NameOps._
import core.Constants._

/** Space logic for checking exhaustivity and unreachability of pattern matching.
 *
 *  The core idea of the algorithm is that patterns and types are value
 *  spaces, which is recursively defined as follows:
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
 */


/** space definition */
sealed trait Space
case object Empty extends Space
case class Typ(tp: Type, decomposed: Boolean) extends Space
case class Kon(tp: Type, params: List[Space]) extends Space
sealed trait Point extends Space
case class Var(sym: Symbol, tp: Type) extends Point
case class Const(value: Constant, tp: Type) extends Point
case class Or(spaces: List[Space]) extends Space

/** abstract space logic */
trait SpaceLogic {
  /** Is `tp1` a subtype of `tp2`? */
  def isSubType(tp1: Type, tp2: Type): Boolean

  /** Is `tp1` the same type as `tp2`? */
  def isEqualType(tp1: Type, tp2: Type): Boolean

  /** Is `tp` a case class? */
  def isCaseClass(tp: Type): Boolean

  /** Is the type `tp` decomposable? i.e. all values of the type can be covered
   *  by its decomposed types.
   *
   * Abstract sealed class, OrType, Boolean and Java enums can be decomposed.
   */
  def canDecompose(tp: Type): Boolean

  /** Return parameters types of the case class `tp` */
  def signature(tp: Type): List[Type]

  /** Get components of decomposable types */
  def partitions(tp: Type): List[Space]

  /** Simplify space using the laws, there's no nested union after simplify */
  def simplify(space: Space): Space = space match {
    case Kon(tp, spaces) =>
      val sp = Kon(tp, spaces.map(simplify _))
      if (sp.params.exists(_ == Empty)) Empty
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
      if (canDecompose(tp) && partitions(tp).isEmpty) Empty
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
  def subspace(a: Space, b: Space): Boolean = (a, b) match {
    case (Empty, _) => true
    case (_, Empty) => false
    case (Or(ss), _) => ss.forall(subspace(_, b))
    case (Typ(tp1, _), Typ(tp2, _)) =>
      isSubType(tp1, tp2)
    case (Typ(tp1, _), Or(ss)) =>
      ss.exists(subspace(a, _)) ||
      (canDecompose(tp1) && subspace(Or(partitions(tp1)), b))
    case (Typ(tp1, _), Kon(tp2, ss)) =>
      isSubType(tp1, tp2) && subspace(Kon(tp2, signature(tp2).map(tp => Typ(tp, false))), b)
    case (Kon(tp1, ss), Typ(tp2, _)) =>
      isSubType(tp1, tp2) ||
      simplify(a) == Empty ||
      (isSubType(tp2, tp1) &&
        canDecompose(tp1) &&
        subspace(Or(partitions(tp1)), b))
    case (Kon(_, _), Or(_)) =>
      simplify(minus(a, b)) == Empty
    case (Kon(tp1, ss1), Kon(tp2, ss2)) =>
      isEqualType(tp1, tp2) && ss1.zip(ss2).forall((subspace _).tupled)
    case (Const(v1, _), Const(v2, _)) => v1 == v2
    case (Const(_, tp1), Typ(tp2, _)) => isSubType(tp1, tp2)
    case (Const(_, _), Or(ss)) => ss.exists(subspace(a, _))
    case (Const(_, _), _) => false
    case (_, Const(_, _)) => false
    case (Var(x, _), Var(y, _)) => x == y
    case (Var(_, tp1), Typ(tp2, _)) => isSubType(tp1, tp2)
    case (Var(_, _), Or(ss)) => ss.exists(subspace(a, _))
    case (Var(_, _), _) => false
    case (_, Var(_, _)) => false
  }

  /** Intersection of two spaces  */
  def intersect(a: Space, b: Space): Space = (a, b) match {
    case (Empty, _) | (_, Empty) => Empty
    case (_, Or(ss)) => Or(ss.map(intersect(a, _)))
    case (Or(ss), _) => Or(ss.map(intersect(_, b)))
    case (Typ(tp1, _), Typ(tp2, _)) =>
      if (isSubType(tp1, tp2)) a
      else if (isSubType(tp2, tp1)) b
      else Empty
    case (Typ(tp1, _), Kon(tp2, ss)) =>
      if (isSubType(tp2, tp1)) b
      else if (isSubType(tp1, tp2)) a
      else Empty
    case (Kon(tp1, ss), Typ(tp2, _)) =>
      if (isSubType(tp1, tp2) || isSubType(tp2, tp1)) a
      else Empty
    case (Kon(tp1, ss1), Kon(tp2, ss2)) =>
      if (!isEqualType(tp1, tp2)) Empty
      else if (ss1.zip(ss2).exists(p => simplify(intersect(p._1, p._2)) == Empty)) Empty
      else
        Kon(tp1, ss1.zip(ss2).map((intersect _).tupled))
    case (Const(v1, _), Const(v2, _)) =>
      if (v1 == v2) a else Empty
    case (Const(_, tp1), Typ(tp2, _)) =>
      if (isSubType(tp1, tp2)) a else Empty
    case (Const(_, _), _) => Empty
    case (Typ(tp1, _), Const(_, tp2)) =>
      if (isSubType(tp2, tp1)) b else Empty
    case (_, Const(_, _)) => Empty
    case (Var(x, _), Var(y, _)) =>
      if (x == y) a else Empty
    case (Var(_, tp1), Typ(tp2, _)) =>
      if (isSubType(tp1, tp2)) a else Empty
    case (Var(_, _), _) => Empty
    case (Typ(tp1, _), Var(_, tp2)) =>
      if (isSubType(tp2, tp1)) b else Empty
    case (_, Var(_, _)) => Empty
  }

  /** The space of a not covered by b */
  def minus(a: Space, b: Space): Space = (a, b) match {
    case (Empty, _) => Empty
    case (_, Empty) => a
    case (Typ(tp1, _), Typ(tp2, _)) =>
      if (isSubType(tp1, tp2)) Empty
      else if (isSubType(tp2, tp1) && canDecompose(tp1))
        minus(Or(partitions(tp1)), b)
      else a
    case (Typ(tp1, _), Kon(tp2, ss)) =>
      if (isSubType(tp1, tp2)) minus(Kon(tp2, signature(tp2).map(tp => Typ(tp, false))), b)
      else if (isSubType(tp2, tp1) && canDecompose(tp1))
        minus(Or(partitions(tp1)), b)
      else a
    case (_, Or(ss)) =>
      ss.foldLeft(a)(minus)
    case (Or(ss), _) =>
      Or(ss.map(minus(_, b)))
    case (Kon(tp1, ss), Typ(tp2, _)) =>
      if (isSubType(tp1, tp2)) Empty
      else if (simplify(a) == Empty) Empty
      else if (isSubType(tp2, tp1) && canDecompose(tp1))
        minus(Or(partitions(tp1)), b)
      else a
    case (Kon(tp1, ss1), Kon(tp2, ss2)) =>
      if (!isEqualType(tp1, tp2)) a
      else if (ss1.zip(ss2).exists(p => simplify(intersect(p._1, p._2)) == Empty)) a
      else if (ss1.zip(ss2).forall((subspace _).tupled)) Empty
      else
        Or(
          ss1.zip(ss2).map((minus _).tupled).zip(0 to ss2.length - 1).map {
            case (ri, i) => Kon(tp1, ss1.updated(i, ri))
          })
    case (Const(v1, _), Const(v2, _)) =>
      if (v1 == v2) Empty else a
    case (Const(_, tp1), Typ(tp2, _)) =>
      if (isSubType(tp1, tp2)) Empty else a
    case (Const(_, _), _) => a
    case (Typ(tp1, _), Const(_, tp2)) =>  // Boolean & Java enum
      if (isSubType(tp2, tp1) && canDecompose(tp1))
        minus(Or(partitions(tp1)), b)
      else a
    case (_, Const(_, _)) => a
    case (Var(x, _), Var(y, _)) =>
      if (x == y) Empty else a
    case (Var(_, tp1), Typ(tp2, _)) =>
      if (isSubType(tp1, tp2)) Empty else a
    case (Var(_, _), _) => a
    case (_, Var(_, _)) => a
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
    case Ident(_) =>
      Typ(pat.tpe.stripAnnots, false)
    case Select(_, _)  =>
      if (pat.symbol.is(Module))
        Typ(pat.tpe.stripAnnots, false)
      else if (pat.symbol.is(Enum))
        Const(Constant(pat.symbol), pat.tpe)
      else
        Var(pat.symbol, pat.tpe)
    case Alternative(trees) => Or(trees.map(project(_, roundUp)))
    case Bind(_, pat) => project(pat)
    case UnApply(_, _, pats) =>
      if (pat.tpe.classSymbol.is(CaseClass))
        Kon(pat.tpe.stripAnnots, pats.map(pat => project(pat, roundUp)))
      else if (roundUp) Typ(pat.tpe, false)
      else Empty
    case Typed(pat @ UnApply(_, _, _), _) => project(pat)
    case Typed(expr, _) => Typ(expr.tpe.stripAnnots, true)
    case _ =>
      Empty
  }

  /** Is `tp1` a subtype of `tp2`?
   *
   *  Ignore type parameters in comparison due to erasure, i.e., Some[Int] <: Some[T]
   */
  def isSubType(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
    case (tp1: RefinedType, tp2: RefinedType) => isSubType(tp1.parent, tp2.parent)
    case (tp1: RefinedType, _) => isSubType(tp1.parent, tp2)
    case (_, tp2: RefinedType) => isSubType(tp1, tp2.parent)
    case (_, _) => tp1 <:< tp2
  }

  def isEqualType(tp1: Type, tp2: Type): Boolean = tp1 =:= tp2

  def signature(tp: Type): List[Type] = {
    val ktor = tp.classSymbol.primaryConstructor.info

    val meth =
      if (ktor.isInstanceOf[MethodType]) ktor
      else
      tp match {
        case AppliedType(_, params) =>
          val refined = params.map {
            // TypeBounds would generate an exception
            case tp: TypeBounds => tp.underlying
            case tp => tp
          }
          ktor.appliedTo(refined)
        case _ =>
          ktor
      }

    meth.firstParamTypes.map(_.stripTypeVar).map(refine(tp, _))
  }

  def partitions(tp: Type): List[Space] = {
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
            refine(tp, sym.asClass.classInfo.symbolicTypeRef)
          else
            sym.info
        } filter(_ <:< tp) // child may not always be subtype of parent: SI-4020

        parts.map(tp => Typ(tp, true))
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
    case (TypeRef(ref1: TypeProxy, _), tp2 @ TypeRef(ref2: TypeProxy, name)) =>
      if (ref1.underlying <:< ref2.underlying) TypeRef(ref1, name) else tp2
    case _ => tp2
  }

  /** Abstract sealed types,  or-types, Boolean and Java enums can be decomposed */
  def canDecompose(tp: Type): Boolean =
    tp.typeSymbol.is(allOf(Abstract, Sealed)) ||
      tp.typeSymbol.is(allOf(Trait, Sealed)) ||
      tp.isInstanceOf[OrType] ||
      tp =:= ctx.definitions.BooleanType ||
      tp.typeSymbol.is(Enum)

  def isCaseClass(tp: Type): Boolean = tp.classSymbol.isClass && tp.classSymbol.is(CaseClass)

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
        if (pre.isEmpty) tp.name.show.stripSuffix("$")
        else pre + "." + tp.name.show.stripSuffix("$")
      case _ => tp.show.stripSuffix("$")
    }

    val text = tp match {
      case tp: OrType => showType(tp.tp1) + " | " + showType(tp.tp2)
      case _ => refine(tp)
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
          showType(tp) + signature(tp).map(_ => "_").mkString("(", ",", ")")
        else if (signature(tp).nonEmpty)
          tp.classSymbol.name + signature(tp).map(_ => "_").mkString("(", ",", ")")
        else if (decomposed) "_: " + showType(tp)
        else "_"
      case Kon(tp, params) =>
        if (ctx.definitions.isTupleType(tp))
          "(" + params.map(p => doShow(p)).mkString(", ") + ")"
        else if (tp.widen.classSymbol.showFullName == "scala.collection.immutable.::")
          if (mergeList) params.map(p => doShow(p, mergeList)).mkString(", ")
          else params.map(p => doShow(p, true)).mkString("List(", ", ", ")")
        else
          showType(tp) + params.map(p => doShow(p)).mkString("(", ", ", ")")
      case Or(_) =>
        throw new Exception("incorrect flatten result " + s)
    }

    flatten(s).map(doShow(_, false)).distinct.mkString(", ")
  }

  def checkable(tp: Type): Boolean = tp match {
    case AnnotatedType(tp, annot) =>
      (ctx.definitions.UncheckedAnnot != annot.symbol) && checkable(tp)
    case _ => true  // actually everything is checkable unless @unchecked

      // tp.classSymbol.is(Sealed) ||
      //   tp.isInstanceOf[OrType] ||
      //   tp.classSymbol.is(Enum) ||
      //   Boolean
      //   Int
      //   ...
  }

  def checkExhaustivity(_match: Match): Unit = {
    val Match(sel, cases) = _match
    val selTyp = sel.tpe.widen.elimAnonymousClass

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
    val selTyp = sel.tpe.widen.elimAnonymousClass

    // starts from the second, the first can't be redundant
    (1 until cases.length).foreach { i =>
      // in redundancy check, take guard as false, take extractor as match
      // nothing in order to soundly approximate
      val prevs = cases.take(i).map { x =>
        if (x.guard.isEmpty) project(x.pat, false)
        else Empty
      }.reduce((a, b) => Or(List(a, b)))

      val curr = project(cases(i).pat)

      if (subspace(curr, prevs)) {
        ctx.warning("unreachable code", cases(i).body.pos)
      }
    }
  }
}
