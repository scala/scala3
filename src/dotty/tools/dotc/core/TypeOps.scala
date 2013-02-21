package dotty.tools.dotc.core

import Contexts._, Types._, Symbols._, Names._, Flags._, Scopes._

trait TypeOps { this: Context =>

  final def asSeenFrom(tp: Type, pre: Type, clazz: Symbol, theMap: AsSeenFromMap): Type = {

    def skipPrefixOf(pre: Type, clazz: Symbol) =
      (pre eq NoType) || (pre eq NoPrefix) || clazz.isPackageClass

    def toPrefix(pre: Type, clazz: Symbol, thisclazz: ClassSymbol): Type =
      if (skipPrefixOf(pre, clazz))
        tp
      else if ((thisclazz isNonBottomSubClass clazz) &&
        (pre.widen.typeSymbol isNonBottomSubClass thisclazz))
        pre match {
          case SuperType(thispre, _) => thispre
          case _ => pre
        }
      else
        toPrefix(pre.baseType(clazz).normalizedPrefix, clazz.owner, thisclazz)

    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (sym.isStatic) tp
        else {
          val pre0 = tp.prefix
          val pre1 = asSeenFrom(pre0, pre, clazz, theMap)
          if (pre1 eq pre0) tp
          else {
            val tp1 = NamedType(pre1, tp.name)
            if (sym is TypeParam) {
              // short-circuit instantiated type parameters
              // by replacing pre.tp with its alias, if it has one.
              val tp2 = tp1.info
              if (tp2.isAliasTypeBounds) return tp2.bounds.hi
            }
            tp1
          }
        }
      case ThisType(thisclazz) =>
        toPrefix(pre, clazz, thisclazz)
      case _: BoundType | NoPrefix =>
        tp
      case tp: RefinedType =>
        tp.derivedRefinedType(
            asSeenFrom(tp.parent, pre, clazz, theMap),
            tp.name,
            asSeenFrom(tp.info, pre, clazz, theMap))
      case _ =>
        (if (theMap != null) theMap else new AsSeenFromMap(pre, clazz))
          .mapOver(tp)
    }
  }

  class AsSeenFromMap(pre: Type, clazz: Symbol) extends TypeMap {
    def apply(tp: Type) = asSeenFrom(tp, pre, clazz, this)
  }

  final def isVolatile(tp: Type): Boolean = {
    def isAbstractIntersection(tp: Type): Boolean = tp match {
      case tp: TypeRef => tp.isAbstractType
      case AndType(l, r) => isAbstractIntersection(l) | isAbstractIntersection(l)
      case OrType(l, r) => isAbstractIntersection(l) & isAbstractIntersection(r)
      case _ => false
    }
    def test = {
      tp match {
        case ThisType(_) =>
          false
        case tp: RefinedType =>
          tp.parent.isVolatile ||
            isAbstractIntersection(tp.parent) &&
            (tp.abstractMemberNames contains tp.name)
        case tp: TypeProxy =>
          tp.underlying.isVolatile
        case AndType(l, r) =>
          l.isVolatile || r.isVolatile ||
            isAbstractIntersection(l) && r.abstractMemberNames(tp).nonEmpty
        case OrType(l, r) =>
          l.isVolatile && r.isVolatile
        case _ =>
          false
      }
    }
    // need to be careful not to fall into an infinite recursion here
    // because volatile checking is done before all cycles are detected.
    // the case to avoid is an abstract type directly or
    // indirectly upper-bounded by itself. See #2918
    try {
      ctx.volatileRecursions += 1
      if (ctx.volatileRecursions < LogVolatileThreshold)
        test
      else if (ctx.pendingVolatiles(tp))
        false // we can return false here, because a cycle will be detected
      // here afterwards and an error will result anyway.
      else
        try {
          ctx.pendingVolatiles += tp
          test
        } finally {
          ctx.pendingVolatiles -= tp
        }
    } finally {
      ctx.volatileRecursions -= 1
    }
  }

  final def glb(tp1: Type, tp2: Type): Type =
    if (tp1 eq tp2) tp1
    else if (tp1.isWrong) tp2
    else if (tp2.isWrong) tp1
    else tp2 match {
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
    else if (tp1.isWrong) tp1
    else if (tp2.isWrong) tp2
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

  /** Merge `t1` into `tp2` if t1 is a subtype of some part of tp2.
   */
  private def mergeIfSub(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
    if (tp1 <:< tp2)
      if (tp2 <:< tp1) tp2 else tp1
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

  /** Merge `tp1` into `tp2` if tp1 is a supertype of some part of tp2.
   */
  private def mergeIfSuper(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
    if (tp2 <:< tp1)
      if (tp1 <:< tp2) tp2 else tp1
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
   *  definitions in scope `decls`.
   */
  def normalizeToRefs(parents: List[Type], cls: ClassSymbol, decls: Scope): List[TypeRef] = {
    var refinements = Map[TypeName, Type]()
    var formals = Map[TypeName, Symbol]()
    def normalizeToRef(tp: Type): TypeRef = tp match {
      case tp @ RefinedType(tp1, name: TypeName) =>
        refinements = refinements.updated(name,
          refinements get name match {
            case Some(info) => info & tp.info
            case none => tp.info
          })
        formals = formals.updated(name, tp1.member(name).symbol)
        normalizeToRef(tp1)
      case tp: TypeRef =>
        tp
      case _ =>
        throw new TypeError(s"unexpected parent type: $tp")
    }
    val parentRefs = parents map normalizeToRef
    for ((name, tpe) <- refinements) decls.enter {
      val formal = formals(name)
      val bounds = tpe.toRHS(formal)
      ctx.newSymbol(cls, name, formal.flags & RetainedTypeArgFlags, bounds)
    }
    parentRefs
  }
}

