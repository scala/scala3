package dotty.tools.dotc.core

import Contexts._, Types._, Symbols._

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
          case SuperType(thistp, _) => thistp
          case _ => pre
        }
      else
        toPrefix(pre.baseType(clazz).normalizedPrefix, clazz.owner, thisclazz)

    def toInstance(pre: Type, clazz: Symbol, tparam: Symbol): Type = {
      if (skipPrefixOf(pre, clazz)) tp
      else {
        val tparamOwner = tparam.owner

        def throwError =
          if (tparamOwner.info.parents exists (_.isErroneous))
            ErrorType // don't be overzealous with throwing exceptions, see #2641
          else
            throw new Error(
              s"something is wrong (wrong class file?): ${tparam.showLocated} cannot be instantiated from ${pre.widen.show}")

        def prefixMatches = pre.typeSymbol isNonBottomSubClass tparamOwner

        val basePre = pre.baseType(clazz)

        def instParamFrom(typeInst: Type): Type = typeInst match {
          case ConstantType(_) =>
            // have to deconst because it may be a Class[T].
            instParamFrom(typeInst.deconst)
          case AppliedType(tycon, baseArgs) =>
            instParam(tycon.typeParams, baseArgs)
          case _ =>
            throwError
        }

        def instParam(ps: List[Symbol], as: List[Type]): Type =
          if (as.isEmpty) tp
          else if (ps.isEmpty) throwError
          else if (tparam eq ps.head)
            if (as.head.exists) as.head else tp
          else instParam(ps.tail, as.tail)

        if (tparamOwner == clazz && prefixMatches) instParamFrom(basePre)
        else toInstance(basePre.normalizedPrefix, clazz.owner, tparam)
      }
    }

    tp match {
      case tp: NamedType =>
        val sym = tp.symbol
        if (tp.symbol.isTypeParameter) toInstance(pre, clazz, sym)
        else if (sym.isStatic) tp
        else tp.derivedNamedType(asSeenFrom(tp.prefix, pre, clazz, theMap), tp.name)
      case ThisType(thisclazz) =>
        toPrefix(pre, clazz, thisclazz)
      case _ =>
        val asSeenFromMap = if (theMap != null) theMap else new AsSeenFromMap(pre, clazz)
        tp match {
          case tp: AppliedType =>
            tp.derivedAppliedType(
              asSeenFromMap(tp.tycon), tp.targs mapConserve asSeenFromMap)
          case _ =>
            asSeenFromMap mapOver tp
        }
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
          case RefinedType(p, names) =>
            p.isVolatile ||
              isAbstractIntersection(p) &&
              (names exists (tp.abstractMemberNames(tp) contains))
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
      import ctx.root.{volatileRecursions, pendingVolatiles}
      try {
        volatileRecursions += 1
        if (volatileRecursions < LogVolatileThreshold)
          test
        else if (pendingVolatiles(tp))
          false // we can return false here, because a cycle will be detected
                // here afterwards and an error will result anyway.
        else
          try {
            pendingVolatiles += tp
            test
          } finally {
            pendingVolatiles -= tp
          }
      } finally {
        volatileRecursions -= 1
      }
    }

    final def glb (tp1: Type, tp2: Type): Type =
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

    def lub (tp1: Type, tp2: Type): Type =
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
}

