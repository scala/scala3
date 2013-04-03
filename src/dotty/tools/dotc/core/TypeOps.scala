package dotty.tools.dotc.core

import Contexts._, Types._, Symbols._, Names._, Flags._, Scopes._

trait TypeOps { this: Context =>

  def newSkolemSingleton(underlying: Type) = TermRef(NoPrefix, newSkolem(underlying))

  final def asSeenFrom(tp: Type, pre: Type, cls: Symbol, theMap: AsSeenFromMap): Type = {

    def toPrefix(pre: Type, cls: Symbol, thiscls: ClassSymbol): Type =
      if ((pre eq NoType) || (pre eq NoPrefix) || (cls is PackageClass))
        tp
      else if (thiscls.isNonBottomSubClass(cls) && pre.baseType(thiscls).exists)
        pre match {
          case SuperType(thispre, _) => thispre
          case _ => pre
        }
      else
        toPrefix(pre.baseType(cls).normalizedPrefix, cls.owner, thiscls)

    /* !!! DEBUG ctx.traceIndented(s"$tp.asSeenFrom($pre, $cls)") */ {
    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (sym.isStatic) tp
        else {
          val tp1 = tp.derivedNamedType(asSeenFrom(tp.prefix, pre, cls, theMap))
          // short-circuit instantiated type parameters
          if ((tp1 ne tp) && (sym is (TypeParam, butNot = Deferred))) tp1.dealias
          else tp1
        }
      case ThisType(thiscls) =>
        toPrefix(pre, cls, thiscls)
      case _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(
            asSeenFrom(tp.parent, pre, cls, theMap),
            tp.refinedName,
            asSeenFrom(tp.refinedInfo, pre, cls, theMap))
      case _ =>
        (if (theMap != null) theMap else new AsSeenFromMap(pre, cls))
          .mapOver(tp)
    }
  }
  } 

  class AsSeenFromMap(pre: Type, cls: Symbol) extends TypeMap {
    def apply(tp: Type) = asSeenFrom(tp, pre, cls, this)
  }

  final def isVolatile(tp: Type): Boolean = {
    /** Pre-filter to avoid expensive DNF computation */
    def needsChecking(tp: Type, isPart: Boolean): Boolean = tp match {
      case tp: TypeRef =>
        tp.info match {
          case TypeBounds(lo, hi) =>
            if (lo eq hi) needsChecking(hi, isPart)
            else isPart || tp.controlled(isVolatile(hi))
          case _ => false
        }
      case tp: RefinedType =>
        needsChecking(tp.parent, true)
      case tp: TypeProxy =>
        needsChecking(tp.underlying, isPart)
      case AndType(l, r) =>
        needsChecking(l, true) || needsChecking(r, true)
      case OrType(l, r) =>
        isPart || needsChecking(l, isPart) && needsChecking(r, isPart)
      case _ =>
        false
    }
    needsChecking(tp, false) && {
      tp.DNF forall { case (parents, refinedNames) =>
        val absParents = parents filter (_.symbol is Deferred)
        absParents.size >= 2 || {
          val ap = absParents.head
          (parents exists (p =>
            (p ne ap) || p.abstractMemberNames(tp).nonEmpty)) ||
          (refinedNames & tp.abstractMemberNames()).nonEmpty ||
          isVolatile(ap)
        }
      }
    }
  }

  final def glb(tp1: Type, tp2: Type): Type =
    if (tp1 eq tp2) tp1
    else tp2 match {  // normalize to disjunctive normal form if possible.
      case OrType(tp21, tp22) =>
        tp1 & tp21 | tp1 & tp22
      case _ =>
        tp1 match {
          case OrType(tp11, tp12) =>
            tp11 & tp2 | tp12 & tp2
          case _ =>
            val t1 = mergeIfSub(tp1, tp2)
            if (t1.exists) t1
            else {
              val t2 = mergeIfSub(tp2, tp1)
              if (t2.exists) t2
              else AndType(tp1, tp2)
            }
        }
    }

  final def glb(tps: List[Type]): Type =
    (defn.AnyType /: tps)(glb)

  def lub(tp1: Type, tp2: Type): Type =
    if (tp1 eq tp2) tp1
    else {
      val t1 = mergeIfSuper(tp1, tp2)
      if (t1.exists) t1
      else {
        val t2 = mergeIfSuper(tp2, tp1)
        if (t2.exists) t2
        else OrType(tp1, tp2)
      }
    }

  final def lub(tps: List[Type]): Type =
    (defn.NothingType /: tps)(lub)

  /** Merge `t1` into `tp2` if t1 is a subtype of some &-summand of tp2.
   */
  private def mergeIfSub(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
    if (tp1 <:< tp2)
      if (tp2 <:< tp1) tp2 else tp1 // keep existing type if possible
    else tp2 match {
      case tp2 @ AndType(tp21, tp22) =>
        val lower1 = mergeIfSub(tp1, tp21)
        if (lower1 eq tp21) tp2
        else if (lower1.exists) lower1 & tp22
        else {
          val lower2 = mergeIfSub(tp1, tp22)
          if (lower2 eq tp22) tp2
          else if (lower2.exists) tp21 & lower2
          else NoType
        }
      case _ =>
        NoType
    }

  /** Merge `tp1` into `tp2` if tp1 is a supertype of some |-summand of tp2.
   */
  private def mergeIfSuper(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
    if (tp2 <:< tp1)
      if (tp1 <:< tp2) tp2 else tp1 // keep existing type if possible
    else tp2 match {
      case tp2 @ OrType(tp21, tp22) =>
        val higher1 = mergeIfSuper(tp1, tp21)
        if (higher1 eq tp21) tp2
        else if (higher1.exists) higher1 | tp22
        else {
          val higher2 = mergeIfSuper(tp1, tp22)
          if (higher2 eq tp22) tp2
          else if (higher2.exists) tp21 | higher2
          else NoType
        }
      case _ =>
        NoType
    }

  /** Normalize a list of parent types of class `cls` that may contain refinements
   *  to a list of typerefs, by converting all refinements to member
   *  definitions in scope `decls`. Can add members to `decls` as a side-effect.
   */
  def normalizeToRefs(parents: List[Type], cls: ClassSymbol, decls: Scope): List[TypeRef] = {
    // println(s"normalizing $parents of $cls in ${cls.owner}") // !!! DEBUG
    var refinements = Map[TypeName, Type]()
    var formals = Map[TypeName, Symbol]()
    def normalizeToRef(tp: Type): TypeRef = tp match {
      case tp @ RefinedType(tp1, name: TypeName) =>
        refinements = refinements.updated(name,
          refinements get name match {
            case Some(info) => info & tp.refinedInfo
            case none => tp.refinedInfo
          })
        formals = formals.updated(name, tp1.typeParamNamed(name))
        normalizeToRef(tp1)
      case tp: TypeRef =>
        tp
      case _ =>
        throw new TypeError(s"unexpected parent type: $tp")
    }
    val parentRefs = parents map normalizeToRef
    for ((name, tpe) <- refinements) {
      val formal = formals(name)
      val bounds = tpe //.toRHS(formal)
      assert(decls.lookup(name) == NoSymbol, // DEBUG
        s"redefinition of ${decls.lookup(name).debugString} in ${cls.showLocated}")
      val sym = ctx.newSymbol(cls, name, formal.flags & RetainedTypeArgFlags, bounds)
      cls.enter(sym, decls)
    }
    parentRefs
  }
}

