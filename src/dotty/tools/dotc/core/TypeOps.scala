package dotty.tools.dotc.core

import Contexts._, Types._, Symbols._, Names._, Flags._, Scopes._

trait TypeOps { this: Context =>

  def newSkolemSingleton(underlying: Type) = TermRef.withSym(NoPrefix, newSkolem(underlying))

  final def asSeenFrom(tp: Type, pre: Type, cls: Symbol, theMap: AsSeenFromMap): Type = {

    def toPrefix(pre: Type, cls: Symbol, thiscls: ClassSymbol): Type = ctx.debugTraceIndented(s"toPrefix($pre, $cls, $thiscls)")  {
      if ((pre eq NoType) || (pre eq NoPrefix) || (cls is PackageClass))
        tp
      else if (thiscls.derivesFrom(cls) && pre.baseType(thiscls).exists)
        pre match {
          case SuperType(thispre, _) => thispre
          case _ => pre
        }
      else
        toPrefix(pre.baseType(cls).normalizedPrefix, cls.owner, thiscls)
    }

    ctx.debugTraceIndented(s"$tp.asSeenFrom($pre, $cls)") { // !!! DEBUG
      tp match {
        case tp: NamedType =>
          val sym = tp.symbol
          if (sym.isStatic) tp
          else {
            val tp1 = tp.derivedNamedType(asSeenFrom(tp.prefix, pre, cls, theMap))
            // Here's an explanation why we short-circuit instantiated type parameters.
            // Say you have             This is translated to:
            //
            // class List[type T] ==>   class List { type T; val hd: T }
            // xs: List[Int]      ==>   List { type T = Int }
            //
            // Then with the line above, xs.hd would have type xs.T
            //
            // But in Scala 2.x, its type is Int, which is the dealiased version
            // of xs.T. With the logic below, we get the same outcome as for 2.x.
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
 //       case tp: ClassInfo => !!! disabled for now
 //         tp.derivedClassInfo(asSeenFrom(tp.prefix, pre, cls, theMap))
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
      case ErrorType =>
        defn.AnyClass.typeConstructor
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

