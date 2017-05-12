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
import reporting.diagnostic.messages._
import config.Printers.{ exhaustivity => debug }

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
 *      5. `Fun(S1, S2, ..., Sn)` is an extractor space.
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

/** Space representing an extractor pattern */
case class Fun(tp: Type, fun: Type, params: List[Space]) extends Space

/** Union of spaces */
case class Or(spaces: List[Space]) extends Space

/** abstract space logic */
trait SpaceLogic {
  /** Is `tp1` a subtype of `tp2`? */
  def isSubType(tp1: Type, tp2: Type): Boolean

  /** Is `tp1` the same type as `tp2`? */
  def isEqualType(tp1: Type, tp2: Type): Boolean

  /** Return a space containing the values of both types.
   *
   * The types should be atomic (non-decomposable) and unrelated (neither
   * should be a subtype of the other).
   */
  def intersectUnrelatedAtomicTypes(tp1: Type, tp2: Type): Space

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

  /** Display space in string format */
  def show(sp: Space): String

  /** Simplify space using the laws, there's no nested union after simplify */
  def simplify(space: Space, aggressive: Boolean = false): Space = space match {
    case Kon(tp, spaces) =>
      val sp = Kon(tp, spaces.map(simplify(_)))
      if (sp.params.contains(Empty)) Empty
      else sp
    case Fun(tp, fun, spaces) =>
      val sp = Fun(tp, fun, spaces.map(simplify(_)))
      if (sp.params.contains(Empty)) Empty
      else sp
    case Or(spaces) =>
      val set = spaces.map(simplify(_)).flatMap {
        case Or(ss) => ss
        case s => Seq(s)
      } filter (_ != Empty)

      if (set.isEmpty) Empty
      else if (set.size == 1) set.toList(0)
      else if (aggressive && spaces.size < 5) {
        val res = set.map(sp => (sp, set.filter(_ ne sp))).find {
          case (sp, sps) =>
            isSubspace(sp, Or(sps))
        }
        if (res.isEmpty) Or(set)
        else simplify(Or(res.get._2), aggressive)
      }
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

    val res = (simplify(a), b) match {
      case (Empty, _) => true
      case (_, Empty) => false
      case (Or(ss), _) =>
        ss.forall(isSubspace(_, b))
      case (Typ(tp1, _), Typ(tp2, _)) =>
        isSubType(tp1, tp2)
      case (Typ(tp1, _), Or(ss)) =>  // optimization
        ss.exists(isSubspace(a, _)) || tryDecompose1(tp1)
      case (_, Or(_)) =>
        simplify(minus(a, b)) == Empty
      case (Typ(tp1, _), Kon(tp2, ss)) =>
        isSubType(tp1, tp2) && isSubspace(Kon(tp2, signature(tp2).map(Typ(_, false))), b)
      case (Kon(tp1, ss), Typ(tp2, _)) =>
        isSubType(tp1, tp2)
      case (Kon(tp1, ss1), Kon(tp2, ss2)) =>
        isEqualType(tp1, tp2) && ss1.zip(ss2).forall((isSubspace _).tupled)
      case (Fun(tp1, fun, ss), Typ(tp2, _)) =>
        isSubType(tp1, tp2)
      case (Typ(tp2, _), Fun(tp1, fun, ss)) =>
        false  // approximation: assume a type can never be fully matched by an extractor
      case (Kon(_, _), Fun(_, _, _)) =>
        false  // approximation
      case (Fun(_, _, _), Kon(_, _)) =>
        false // approximation
      case (Fun(_, fun1, ss1), Fun(_, fun2, ss2)) =>
        isEqualType(fun1, fun2) && ss1.zip(ss2).forall((isSubspace _).tupled)
    }

    debug.println(s"${show(a)} < ${show(b)} = $res")

    res
  }

  /** Intersection of two spaces  */
  def intersect(a: Space, b: Space): Space = {
    def tryDecompose1(tp: Type) = intersect(Or(decompose(tp)), b)
    def tryDecompose2(tp: Type) = intersect(a, Or(decompose(tp)))

    val res: Space = (a, b) match {
      case (Empty, _) | (_, Empty) => Empty
      case (_, Or(ss)) => Or(ss.map(intersect(a, _)).filterConserve(_ ne Empty))
      case (Or(ss), _) => Or(ss.map(intersect(_, b)).filterConserve(_ ne Empty))
      case (Typ(tp1, _), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) a
        else if (isSubType(tp2, tp1)) b
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else intersectUnrelatedAtomicTypes(tp1, tp2)
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
      case (Typ(tp1, _), Fun(tp2, _, _)) =>
        if (isSubType(tp1, tp2) || isSubType(tp2, tp1)) b  // prefer extractor space for better approximation
        else if (canDecompose(tp1)) tryDecompose1(tp1)
        else Empty
      case (Fun(tp1, _, _), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2) || isSubType(tp2, tp1)) a
        else if (canDecompose(tp2)) tryDecompose2(tp2)
        else Empty
      case (Fun(tp1, _, _), Kon(tp2, _)) =>
        if (isSubType(tp1, tp2) || isSubType(tp2, tp1)) a
        else Empty
      case (Kon(tp1, _), Fun(tp2, _, _)) =>
        if (isSubType(tp1, tp2) || isSubType(tp2, tp1)) b
        else Empty
      case (Fun(tp1, fun1, ss1), Fun(tp2, fun2, ss2)) =>
        if (!isEqualType(fun1, fun2)) Empty
        else if (ss1.zip(ss2).exists(p => simplify(intersect(p._1, p._2)) == Empty)) Empty
        else Fun(tp1, fun1, ss1.zip(ss2).map((intersect _).tupled))
    }

    debug.println(s"${show(a)} & ${show(b)} = ${show(res)}")

    res
  }

  /** The space of a not covered by b */
  def minus(a: Space, b: Space): Space = {
    def tryDecompose1(tp: Type) = minus(Or(decompose(tp)), b)
    def tryDecompose2(tp: Type) = minus(a, Or(decompose(tp)))

    val res = (a, b) match {
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
      case (Fun(tp1, _, _), Typ(tp2, _)) =>
        if (isSubType(tp1, tp2)) Empty
        else a
      case (Typ(tp1, _), Fun(tp2, _, _)) =>
        a  // approximation
      case (Fun(_, _, _), Kon(_, _)) =>
        a
      case (Kon(_, _), Fun(_, _, _)) =>
        a
      case (Fun(tp1, fun1, ss1), Fun(tp2, fun2, ss2)) =>
        if (!isEqualType(fun1, fun2)) a
        else if (ss1.zip(ss2).exists(p => simplify(intersect(p._1, p._2)) == Empty)) a
        else if (ss1.zip(ss2).forall((isSubspace _).tupled)) Empty
        else
          // `(_, _, _) - (Some, None, _)` becomes `(None, _, _) | (_, Some, _) | (_, _, Empty)`
          Or(ss1.zip(ss2).map((minus _).tupled).zip(0 to ss2.length - 1).map {
              case (ri, i) => Fun(tp1, fun1, ss1.updated(i, ri))
            })

    }

    debug.println(s"${show(a)} - ${show(b)} = ${show(res)}")

    res
  }
}

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
}

/** Scala implementation of space logic */
class SpaceEngine(implicit ctx: Context) extends SpaceLogic {
  import SpaceEngine._
  import tpd._

  private val scalaSomeClass       = ctx.requiredClassRef("scala.Some".toTermName).symbol.asClass
  private val scalaSeqFactoryClass = ctx.requiredClass("scala.collection.generic.SeqFactory".toTypeName)
  private val scalaListType        = ctx.requiredClassRef("scala.collection.immutable.List".toTypeName)
  private val scalaNilType         = ctx.requiredModuleRef("scala.collection.immutable.Nil".toTermName)
  private val scalaConType         = ctx.requiredClassRef("scala.collection.immutable.::".toTypeName)

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

  override def intersectUnrelatedAtomicTypes(tp1: Type, tp2: Type) = {
    val and = AndType(tp1, tp2)
    // Precondition: !(tp1 <:< tp2) && !(tp2 <:< tp1)
    // Then, no leaf of the and-type tree `and` is a subtype of `and`.
    // Then, to create a value of type `and` you must instantiate a trait (class/module)
    // which is a subtype of all the leaves of `and`.
    val imp = implementability(and)

    debug.println(s"atomic intersection: ${and.show} ~ ${imp.show}")

    imp match {
      case Unimplementable => Empty
      case _ => Typ(and, true)
    }
  }

  /** Return the space that represents the pattern `pat`
   */
  def project(pat: Tree): Space = pat match {
    case Literal(c) =>
      if (c.value.isInstanceOf[Symbol])
        Typ(c.value.asInstanceOf[Symbol].termRef, false)
      else
        Typ(ConstantType(c), false)
    case _: BackquotedIdent => Typ(pat.tpe, false)
    case Ident(_) | Select(_, _) =>
      Typ(pat.tpe.stripAnnots, false)
    case Alternative(trees) => Or(trees.map(project(_)))
    case Bind(_, pat) => project(pat)
    case UnApply(fun, _, pats) =>
      if (pat.tpe.classSymbol.is(CaseClass))
        // FIXME: why dealias is needed here?
        Kon(pat.tpe.stripAnnots.dealias, pats.map(pat => project(pat)))
      else if (fun.symbol.owner == scalaSeqFactoryClass && fun.symbol.name == nme.unapplySeq)
        projectList(pats)
      else if (fun.symbol.info.resultType.isRef(scalaSomeClass))
        Kon(pat.tpe.stripAnnots.dealias, pats.map(pat => project(pat)))
      else
        Fun(pat.tpe.stripAnnots.dealias, fun.tpe, pats.map(pat => project(pat)))
    case Typed(pat @ UnApply(_, _, _), _) => project(pat)
    case Typed(expr, _) => Typ(expr.tpe.stripAnnots, true)
    case _ =>
      Empty
  }


  /** Space of the pattern: List(a, b, c: _*)
   */
  def projectList(pats: List[Tree]): Space = {
    if (pats.isEmpty) return Typ(scalaNilType, false)

    val (items, zero) = if (pats.last.tpe.isRepeatedParam)
      (pats.init, Typ(scalaListType.appliedTo(pats.head.tpe.widen), false))
    else
      (pats, Typ(scalaNilType, false))

    items.foldRight[Space](zero) { (pat, acc) =>
      Kon(scalaConType.appliedTo(pats.head.tpe.widen), project(pat) :: acc :: Nil)
    }
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
    val res = erase(tp1) <:< erase(tp2)
    debug.println(s"${tp1.show} <:< ${tp2.show} = $res")
    res
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

    debug.println(s"candidates for ${tp.show} : [${children.map(_.show).mkString(", ")}]")

    tp.dealias match {
      case AndType(tp1, tp2) =>
        intersect(Typ(tp1, false), Typ(tp2, false)) match {
          case Or(spaces) => spaces
          case Empty => Nil
          case space => List(space)
        }
      case OrType(tp1, tp2) => List(Typ(tp1, true), Typ(tp2, true))
      case _ if tp =:= ctx.definitions.BooleanType =>
        List(
          Typ(ConstantType(Constant(true)), true),
          Typ(ConstantType(Constant(false)), true)
        )
      case _ if tp.classSymbol.is(Enum) =>
        children.map(sym => Typ(sym.termRef, true))
      case _ =>
        val parts = children.map { sym =>
          if (sym.is(ModuleClass))
            refine(tp, sym.sourceModule.termRef)
          else if (sym.isTerm)
            refine(tp, sym.termRef)
          else if (sym.info.typeParams.length > 0 || tp.isInstanceOf[TypeRef])
            refine(tp, sym.typeRef)
          else
            sym.typeRef
        } filter { tpe =>
          // Child class may not always be subtype of parent:
          // GADT & path-dependent types
          val res = tpe <:< expose(tp)
          if (!res) debug.println(s"unqualified child ousted: ${tpe.show} !< ${tp.show}")
          res
        }

        debug.println(s"${tp.show} decomposes to [${parts.map(_.show).mkString(", ")}]")

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
    case (tp1: RefinedType, _: TypeRef) => tp1.wrapIfMember(refine(tp1.parent, tp2))
    case (tp1: HKApply, _) => refine(tp1.superType, tp2)
    case (TypeRef(ref1: TypeProxy, _), tp2 @ TypeRef(ref2: TypeProxy, _)) =>
      if (ref1.underlying <:< ref2.underlying) tp2.derivedSelect(ref1) else tp2
    case (TypeRef(ref1: TypeProxy, _), tp2 @ TermRef(ref2: TypeProxy, _)) =>
      if (ref1.underlying <:< ref2.underlying) tp2.derivedSelect(ref1) else tp2
    case _ => tp2
  }

  /** Abstract sealed types, or-types, Boolean and Java enums can be decomposed */
  def canDecompose(tp: Type): Boolean = {
    val dealiasedTp = tp.dealias
    val res = tp.classSymbol.is(allOf(Abstract, Sealed)) ||
      tp.classSymbol.is(allOf(Trait, Sealed)) ||
      dealiasedTp.isInstanceOf[OrType] ||
      (dealiasedTp.isInstanceOf[AndType] && {
        val and = dealiasedTp.asInstanceOf[AndType]
        canDecompose(and.tp1) || canDecompose(and.tp2)
      }) ||
      tp =:= ctx.definitions.BooleanType ||
      tp.classSymbol.is(allOf(Enum, Sealed))  // Enum value doesn't have Sealed flag

    debug.println(s"decomposable: ${tp.show} = $res")

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
      case Typ(c: ConstantType, _) => c.value.show
      case Typ(tp: TermRef, _) => tp.symbol.showName
      case Typ(tp, decomposed) =>
        val sym = tp.widen.classSymbol

        if (ctx.definitions.isTupleType(tp))
          signature(tp).map(_ => "_").mkString("(", ", ", ")")
        else if (sym.showFullName == "scala.collection.immutable.List")
          if (mergeList) "_*" else "_: List"
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
      case Fun(tp, fun, params) =>
        showType(fun) + params.map(doShow(_)).mkString("(", ", ", ")")
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
          (tp.isInstanceOf[AndType] && {
            val and = tp.asInstanceOf[AndType]
            isCheckable(and.tp1) || isCheckable(and.tp2)
          }) ||
          tp.typeSymbol == ctx.definitions.BooleanType.typeSymbol ||
          tp.typeSymbol.is(Enum) ||
          canDecompose(tp) ||
          (defn.isTupleType(tp) && tp.dealias.argInfos.exists(isCheckable(_)))
    }

    val Match(sel, cases) = tree
    val res = isCheckable(sel.tpe.widen.deAnonymize.dealiasKeepAnnots)
    debug.println(s"checkable: ${sel.show} = $res")
    res
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
          case _ =>
            tp.derivedRefinedType(
              expose(tp.parent),
              tp.refinedName,
              tp.refinedInfo
            )
        }
      case _ => tp
    }
  }

  def checkExhaustivity(_match: Match): Unit = {
    val Match(sel, cases) = _match
    val selTyp = sel.tpe.widen.deAnonymize.dealias


    val patternSpace = cases.map({ x =>
      val space = project(x.pat)
      debug.println(s"${x.pat.show} ====> ${show(space)}")
      space
    }).reduce((a, b) => Or(List(a, b)))
    val uncovered = simplify(minus(Typ(selTyp, true), patternSpace), aggressive = true)

    if (uncovered != Empty)
      ctx.warning(PatternMatchExhaustivity(show(uncovered)), sel.pos)
  }

  def checkRedundancy(_match: Match): Unit = {
    val Match(sel, cases) = _match
    // ignore selector type for now
    // val selTyp = sel.tpe.widen.deAnonymize.dealias

    if (cases.length == 1) return

    // starts from the second, the first can't be redundant
    (1 until cases.length).foreach { i =>
      // in redundancy check, take guard as false in order to soundly approximate
      val prevs = cases.take(i).map { x =>
        if (x.guard.isEmpty) project(x.pat)
        else Empty
      }.reduce((a, b) => Or(List(a, b)))

      val curr = project(cases(i).pat)

      debug.println(s"---------------reachable? ${show(curr)}")
      debug.println(s"prev: ${show(prevs)}")

      if (isSubspace(curr, prevs)) {
        ctx.warning(MatchCaseUnreachable(), cases(i).body.pos)
      }
    }
  }
}
