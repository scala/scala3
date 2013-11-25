package dotty.tools.dotc.core

import Contexts._, Types._, Symbols._, Names._, Flags._, Scopes._

trait TypeOps { this: Context =>

  def newSkolemSingleton(underlying: Type) = TermRef(NoPrefix, newSkolem(underlying))

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

    ctx.conditionalTraceIndented(TypeOps.track , s"asSeen ${tp.show} from (${pre.show}, ${cls.show})", show = true) { // !!! DEBUG
      tp match {
        case tp: NamedType =>
          val sym = tp.symbol
          if (sym.isStatic) tp
          else tp.derivedSelect(asSeenFrom(tp.prefix, pre, cls, theMap))
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
          ((parents exists (p =>
                (p ne ap)
             || p.memberNames(abstractTypeNameFilter, tp).nonEmpty
             || p.memberNames(abstractTermNameFilter, tp).nonEmpty))
          || (refinedNames & tp.memberNames(abstractTypeNameFilter, tp)).nonEmpty
          || (refinedNames & tp.memberNames(abstractTermNameFilter, tp)).nonEmpty
          || isVolatile(ap)
          )
        }
      }
    }
  }

  /** Normalize a list of parent types of class `cls` that may contain refinements
   *  to a list of typerefs, by converting all refinements to member
   *  definitions in scope `decls`. Can add members to `decls` as a side-effect.
   */
  def normalizeToRefs(parents: List[Type], cls: ClassSymbol, decls: Scope): List[TypeRef] = {

    def enterArgBinding(formal: Symbol, info: Type) = {
      val typeArgFlag = if (formal is Local) TypeArgument else EmptyFlags
      val sym = ctx.newSymbol(cls, formal.name, formal.flags & RetainedTypeArgFlags | typeArgFlag, info)
      cls.enter(sym, decls)
    }

    /** If we just entered the type argument binding
     *
     *    type From = To
     *
     *  and there is a type argument binding in a parent in `prefs` of the form
     *
     *    type X = From
     *
     *  then also add the binding
     *
     *    type X = To
     *
     *  to the current scope, provided (1) variances of both aliases are the same, and
     *  (2) X is not yet defined in current scope. This "short-circuiting" prevents
     *  long chains of aliases which would have to be traversed in type comparers.
     */
    def forwardRefs(from: Symbol, to: Type, prefs: List[TypeRef]) = to match {
      case to @ TypeBounds(lo1, hi1) if lo1 eq hi1 =>
        for (pref <- prefs)
          for (argSym <- pref.decls)
            if (argSym is TypeArgument) {
              argSym.info match {
                case info @ TypeBounds(lo2 @ TypeRef(ThisType(_), name), hi2) =>
                  if (name == from.name &&
                      (lo2 eq hi2) &&
                      info.variance == to.variance &&
                      !decls.lookup(argSym.name).exists) {
//                    println(s"short-circuit ${argSym.name} was: ${argSym.info}, now: $to")
                    enterArgBinding(argSym, to)
                  }
                case _ =>
              }
            }
      case _ =>
    }

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
        defn.AnyClass.typeRef
      case _ =>
        throw new TypeError(s"unexpected parent type: $tp")
    }
    val parentRefs = parents map normalizeToRef
    for ((name, tpe) <- refinements) {
      assert(decls.lookup(name) == NoSymbol, // DEBUG
        s"redefinition of ${decls.lookup(name).debugString} in ${cls.showLocated}")
      enterArgBinding(formals(name), tpe)
    }
    // These two loops cannot be fused because second loop assumes that
    // all arguments have been entered in `decls`.
    for ((name, tpe) <- refinements) {
      forwardRefs(formals(name), tpe, parentRefs)
    }
    parentRefs
  }
}

object TypeOps {

  var track = false // !!!DEBUG
}
