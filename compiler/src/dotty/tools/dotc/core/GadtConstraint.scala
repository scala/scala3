package dotty.tools
package dotc
package core

import Decorators._
import Contexts._
import Types._
import Symbols._
import util.{SimpleIdentitySet, SimpleIdentityMap}
import collection.mutable
import printing._

import scala.annotation.internal.sharable
import Denotations.{Denotation, SingleDenotation}
import SymDenotations.NoDenotation

/** Types that represent a path. Can either be a TermRef or a SkolemType. */
type PathType = TermRef | SkolemType

/** Represents GADT constraints currently in scope */
sealed abstract class GadtConstraint extends Showable {
  /** Immediate bounds of `sym`. Does not contain lower/upper symbols (see [[fullBounds]]). */
  def bounds(sym: Symbol)(using Context): TypeBounds | Null

  /** Immediate bounds of a path-dependent type.
    * This variant of bounds will ONLY try to retrieve path-dependent GADT bounds. */
  def bounds(path: PathType, sym: Symbol)(using Context): TypeBounds | Null
  def bounds(tp: TypeRef)(using Context): TypeBounds | Null

  /** Full bounds of `sym`, including TypeRefs to other lower/upper symbols.
   *
   * @note this performs subtype checks between ordered symbols.
   *       Using this in isSubType can lead to infinite recursion. Consider `bounds` instead.
   */
  def fullBounds(sym: Symbol)(using Context): TypeBounds | Null

  def fullBounds(path: PathType, sym: Symbol)(using Context): TypeBounds | Null
  def fullBounds(tp: TypeRef)(using Context): TypeBounds | Null

  /** Is `sym1` ordered to be less than `sym2`? */
  def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean

  /** Is `tp1` ordered to be less than `tp2`? */
  def isLess(tp1: NamedType, tp2: NamedType)(using Context): Boolean

  /** Add symbols to constraint, correctly handling inter-dependencies.
   *
   * @see [[ConstraintHandling.addToConstraint]]
   */
  def addToConstraint(syms: List[Symbol])(using Context): Boolean
  def addToConstraint(sym: Symbol)(using Context): Boolean = addToConstraint(sym :: Nil)

  /** Add path to constraint, registering all its abstract type members. */
  def addToConstraint(path: PathType)(using Context): Boolean

  /** Further constrain a symbol already present in the constraint. */
  def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean

  /** Further constrain a path-dependent type already present in the constraint. */
  def addBound(p: PathType, sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean

  def addEquality(p: PathType, q: PathType)(using Context): Unit

  def isEquivalent(p: PathType, q: PathType): Boolean

  def reprOf(p: PathType): PathType | Null

  /** Scrutinee path of the current pattern matching. */
  def scrutineePath: TermRef | Null

  /** Reset scrutinee path to null. */
  def resetScrutineePath(): Unit

  /** Set the scrutinee path. */
  def withScrutineePath[T](path: TermRef | Null)(op: => T): T

  /** Supply the real pattern path. */
  def supplyPatternPath(path: TermRef)(using Context): Unit

  /** Create a skolem type for pattern. */
  def createPatternSkolem(pat: Type): SkolemType

  /** Is the symbol registered in the constraint?
   *
   * @note this is true even if the symbol is constrained to be equal to another type, unlike [[Constraint.contains]].
   */
  def contains(sym: Symbol)(using Context): Boolean

  /** Checks whether a path is registered. */
  def contains(path: PathType)(using Context): Boolean

  /** Checks whether a path-dependent type is registered in the handler. */
  def contains(path: PathType, sym: Symbol)(using Context): Boolean

  /** Checks whether a given path-dependent type is constrainable. */
  def isConstrainablePDT(path: PathType, sym: Symbol)(using Context): Boolean

  def registeredTypeMembers(path: PathType): List[Symbol]

  /** GADT constraint narrows bounds of at least one variable */
  def isNarrowing: Boolean

  /** See [[ConstraintHandling.approximation]] */
  def approximation(sym: Symbol, fromBelow: Boolean, maxLevel: Int = Int.MaxValue)(using Context): Type

  def symbols: List[Symbol]

  def fresh: GadtConstraint

  /** Restore the state from other [[GadtConstraint]], probably copied using [[fresh]] */
  def restore(other: GadtConstraint): Unit

  def debugBoundsDescription(using Context): String
}

final class ProperGadtConstraint private(
  private var myConstraint: Constraint,
  private var mapping: SimpleIdentityMap[Symbol, TypeVar],
  private var reverseMapping: SimpleIdentityMap[TypeParamRef, Symbol],
  private var pathDepMapping: SimpleIdentityMap[PathType, SimpleIdentityMap[Symbol, TypeVar]],
  private var pathDepReverseMapping: SimpleIdentityMap[TypeParamRef, TypeRef],
  private var wasConstrained: Boolean,
  private var myScrutineePath: TermRef | Null,
  private var myUnionFind: SimpleIdentityMap[PathType, PathType],
  private var myPatternSkolem: SkolemType | Null,
) extends GadtConstraint with ConstraintHandling {
  import dotty.tools.dotc.config.Printers.{gadts, gadtsConstr}

  def this() = this(
    myConstraint = new OrderingConstraint(SimpleIdentityMap.empty, SimpleIdentityMap.empty, SimpleIdentityMap.empty, SimpleIdentitySet.empty),
    mapping = SimpleIdentityMap.empty,
    reverseMapping = SimpleIdentityMap.empty,
    pathDepMapping = SimpleIdentityMap.empty,
    pathDepReverseMapping = SimpleIdentityMap.empty,
    wasConstrained = false,
    myScrutineePath = null,
    myUnionFind = SimpleIdentityMap.empty,
    myPatternSkolem = null,
  )

  /** Whether `left` subsumes `right`?
    *
    * `left` and `right` both stem from the constraint `pre`, with different type reasoning performed,
    * during which new types might be registered in GadtConstraint. This function will take such newly
    * registered types into consideration.
    */
  def subsumes(left: GadtConstraint, right: GadtConstraint, pre: GadtConstraint)(using Context): Boolean = {
    def checkSubsumes(c1: Constraint, c2: Constraint, pre: Constraint): Boolean = {
      if (c2 eq pre) true
      else if (c1 eq pre) false
      else {
        val saved = constraint

        def computeNewParams =
          val params1 = c1.domainParams.toSet
          val params2 = c2.domainParams.toSet
          val preParams = pre.domainParams.toSet
          /** Type parameter registered after branching */
          (params1.diff(preParams), params2.diff(preParams))

        val (newParams1, newParams2) = computeNewParams

        // When new types are registered after pre, for left to subsume right, it should contain all types
        // newly registered in right.
        def checkNewParams: Boolean = (left, right) match {
          case (left: ProperGadtConstraint, right: ProperGadtConstraint) =>
            newParams2 forall { p2 =>
              val tp2 = right.externalize(p2)
              left.tvarOf(tp2) != null
            }
          case _ => true
        }

        checkNewParams && {
          // compute mappings between the newly-registered type params in the two branches
          def createMappings = {
            var mapping1: SimpleIdentityMap[TypeParamRef, TypeParamRef] = SimpleIdentityMap.empty
            var mapping2: SimpleIdentityMap[TypeParamRef, TypeParamRef] = SimpleIdentityMap.empty

            (left, right) match {
              case (left: ProperGadtConstraint, right: ProperGadtConstraint) =>
                newParams1 foreach { p1 =>
                  val tp1 = left.externalize(p1)
                  right.tvarOf(tp1) match {
                    case null =>
                    case tvar2: TypeVar =>
                      mapping1 = mapping1.updated(p1, tvar2.origin)
                      mapping2 = mapping2.updated(tvar2.origin, p1)
                  }
                }
              case _ =>
            }

            def mapTypeParam(m: SimpleIdentityMap[TypeParamRef, TypeParamRef])(tpr: TypeParamRef) =
              m(tpr) match
                case null => tpr
                case tpr1: TypeParamRef => tpr1

            (mapTypeParam(mapping1), mapTypeParam(mapping2))
          }

          // bridge between the newly-registered types in c2 and c1
          val (mapping1, mapping2) = createMappings

          try {
            // checks existing type parameters in `pre`
            def existing: Boolean = pre.forallParams { p =>
              c1.contains(p) &&
                c2.upper(p).forall { q =>
                  c1.isLess(p, mapping1(q))
                } && isSubTypeWhenFrozen(c1.nonParamBounds(p), c2.nonParamBounds(p))
            }

            // checks new type parameters in `c1`
            def added: Boolean = newParams1 forall { p1 =>
              val p2 = mapping1(p1)
              c2.upper(p2).forall { q =>
                c1.isLess(p1, mapping2(q))
              } && isSubTypeWhenFrozen(c1.nonParamBounds(p1), c2.nonParamBounds(p2))
            }

            existing && checkNewParams && added
          } finally constraint = saved
        }
      }
    }

    def extractConstraint(g: GadtConstraint) = g match {
      case s: ProperGadtConstraint => s.constraint
      case EmptyGadtConstraint => OrderingConstraint.empty
    }

    checkSubsumes(extractConstraint(left), extractConstraint(right), extractConstraint(pre))
  }

  override protected def legalBound(param: TypeParamRef, rawBound: Type, isUpper: Boolean)(using Context): Type =
    // GADT constraints never involve wildcards and are not propagated outside
    // the case where they're valid, so no approximating is needed.
    rawBound

  /** Whether type members of the given path is constrainable?
   *
   * Package's and module's type members will not be constrained.
   */
  private def isConstrainablePath(path: Type)(using Context): Boolean = path match
    case path: TermRef
      if !path.symbol.is(Flags.Package)
         && !path.symbol.is(Flags.Module)
         && !path.classSymbol.is(Flags.Package)
         && !path.classSymbol.is(Flags.Module)
      => true
    case _: SkolemType
      if !path.classSymbol.is(Flags.Package)
         && !path.classSymbol.is(Flags.Module)
      => true
    case _ => false

  /** Find all constrainable type member denotations of the given type.
   *
   * All abstract but not opaque type members are returned.
   * Note that we return denotation here, since the bounds of the type member
   * depend on the context (e.g. applied type parameters).
   */
  private def constrainableTypeMembers(tp: Type)(using Context): List[Denotation] =
    tp.typeMembers.toList filter { denot =>
      val denot1 = tp.nonPrivateMember(denot.name)
      val tb = denot.info

      def nonPrivate: Boolean = !denot1.isInstanceOf[NoDenotation.type]

      denot1.symbol.is(Flags.Deferred)
      && !denot1.symbol.is(Flags.Opaque)
      && !denot1.symbol.isClass
      && nonPrivate
    }

  private def isConstrainableTypeMember(path: PathType, sym: Symbol)(using Context): Boolean =
    val mbr = path.nonPrivateMember(sym.name)
    mbr.isInstanceOf[SingleDenotation] && {
      val denot1 = path.nonPrivateMember(mbr.name)
      val tb = mbr.info

      denot1.symbol.is(Flags.Deferred)
      && !denot1.symbol.is(Flags.Opaque)
      && !denot1.symbol.isClass
    }

  override def isConstrainablePDT(path: PathType, sym: Symbol)(using Context): Boolean =
    isConstrainablePath(path) && isConstrainableTypeMember(path, sym)

  private def tvarOf(path: PathType, sym: Symbol)(using Context): TypeVar | Null =
    pathDepMapping(path) match
      case null => null
      case innerMapping => innerMapping.nn(sym)

  /** Try to retrieve type variable for some TypeRef.
   * Both type parameters and path-dependent types are considered.
   */
  private def tvarOf(tpr: TypeRef)(using Context): TypeVar | Null =
    mapping(tpr.symbol) match
      case null =>
        tpr match
          case TypeRef(p: PathType, _) => tvarOf(p, tpr.symbol)
          case _ => null
      case tv => tv

  /** Try to retrieve the internal type variable for a NamedType. */
  private def tvarOf(ntp: NamedType)(using Context): TypeVar | Null =
    ntp match
      case tp: TypeRef => tvarOf(tp)
      case _ => null

  private def tvarOf(tp: Type)(using Context): TypeVar | Null =
    tp match
      case tp: TypeRef => tvarOf(tp)
      case _ => null

  override def addToConstraint(path: PathType)(using Context): Boolean = isConstrainablePath(path) && {
    import NameKinds.DepParamName
    val pathType = path.widen
    val typeMembers = constrainableTypeMembers(path).filterNot(_.symbol eq NoSymbol)

    gadts.println(i"> trying to add $path into constraint ...")
    gadts.println(i"  path.widen = $pathType")
    gadts.println(i"  type members =\n${debugShowTypeMembers(typeMembers)}")

    typeMembers.nonEmpty && {
      val typeMemberSymbols: List[Symbol] = typeMembers map { x => x.symbol }

      val poly1 = PolyType(typeMembers map { d => DepParamName.fresh(d.name.toTypeName) })(
        pt => typeMembers map { typeMember =>
          def substDependentSyms(tp: Type, isUpper: Boolean)(using Context): Type = {
            def loop(tp: Type): Type = tp match
              case tp @ AndType(tp1, tp2) if !isUpper =>
                tp.derivedAndOrType(loop(tp1), loop(tp2))
              case tp @ OrType(tp1, tp2) if isUpper =>
                tp.derivedOrType(loop(tp1), loop(tp2))
              case tp @ TypeRef(prefix, des) if prefix eq path =>
                typeMemberSymbols indexOf tp.symbol match
                  case -1 => tp
                  case idx => pt.paramRefs(idx)
              case tp @ TypeRef(_: RecThis, des) =>
                typeMemberSymbols indexOf tp.symbol match
                  case -1 => tp
                  case idx => pt.paramRefs(idx)
              case tp: TypeRef =>
                tvarOf(tp) match {
                  case tv: TypeVar => tv.origin
                  case null => tp
                }
              case tp => tp

            loop(tp)
          }

          val tb = typeMember.info.bounds

          def stripLazyRef(tp: Type): Type = tp match
            case tp @ RefinedType(parent, name, tb) =>
              tp.derivedRefinedType(stripLazyRef(parent), name, stripLazyRef(tb))
            case tp: RecType =>
              tp.derivedRecType(stripLazyRef(tp.parent))
            case tb: TypeBounds =>
              tb.derivedTypeBounds(stripLazyRef(tb.lo), stripLazyRef(tb.hi))
            case ref: LazyRef =>
              ref.stripLazyRef
            case _ => tp

          val tb1: TypeBounds = stripLazyRef(tb).asInstanceOf

          tb1.derivedTypeBounds(
            lo = substDependentSyms(tb1.lo, isUpper = false),
            hi = substDependentSyms(tb1.hi, isUpper = true)
          )
        },
        pt => defn.AnyType
      )

      def register: Boolean =
        val tvars = typeMemberSymbols lazyZip poly1.paramRefs map { (sym, paramRef) =>
          val tv = TypeVar(paramRef, creatorState = null)

          val externalType = TypeRef(path, sym)
          pathDepMapping = pathDepMapping.updated(path, {
            val old: SimpleIdentityMap[Symbol, TypeVar] = pathDepMapping(path) match
              case null => SimpleIdentityMap.empty
              case m => m.nn

            old.updated(sym, tv)
          })
          pathDepReverseMapping = pathDepReverseMapping.updated(tv.origin, externalType)

          tv
        }

        addToConstraint(poly1, tvars)
          .showing(i"added to constraint: [$poly1] $path\n$debugBoundsDescription", gadts)

      register
    }
  }

  private def debugShowTypeMembers(typeMembers: List[Denotation])(using Context): String =
    val buf = new mutable.StringBuilder
    buf ++= "{\n"
    typeMembers foreach { denot =>
      buf ++= i"  ${denot.symbol}: ${denot.info.bounds} [ ${denot.info.bounds.toString} ]\n"
    }
    buf ++= "}"
    buf.result

  override def reprOf(p: PathType): PathType | Null = lookupPath(p)

  override def addToConstraint(params: List[Symbol])(using Context): Boolean = {
    import NameKinds.DepParamName

    val poly1 = PolyType(params.map { sym => DepParamName.fresh(sym.name.toTypeName) })(
      pt => params.map { param =>
        // In bound type `tp`, replace the symbols in dependent positions with their internal TypeParamRefs.
        // The replaced symbols will be later picked up in `ConstraintHandling#addToConstraint`
        // and used as orderings.
        def substDependentSyms(tp: Type, isUpper: Boolean)(using Context): Type = {
          def loop(tp: Type) = substDependentSyms(tp, isUpper)
          tp match {
            case tp @ AndType(tp1, tp2) if !isUpper =>
              tp.derivedAndType(loop(tp1), loop(tp2))
            case tp @ OrType(tp1, tp2) if isUpper =>
              tp.derivedOrType(loop(tp1), loop(tp2))
            case tp: NamedType =>
              params.indexOf(tp.symbol) match {
                case -1 =>
                  mapping(tp.symbol) match {
                    case tv: TypeVar => tv.origin
                    case null => tp
                  }
                case i => pt.paramRefs(i)
              }
            case tp => tp
          }
        }

        val tb = param.info.bounds
        tb.derivedTypeBounds(
          lo = substDependentSyms(tb.lo, isUpper = false),
          hi = substDependentSyms(tb.hi, isUpper = true)
        )
      },
      pt => defn.AnyType
    )

    val tvars = params.lazyZip(poly1.paramRefs).map { (sym, paramRef) =>
      val tv = TypeVar(paramRef, creatorState = null)
      mapping = mapping.updated(sym, tv)
      reverseMapping = reverseMapping.updated(tv.origin, sym)
      tv
    }

    // The replaced symbols are picked up here.
    addToConstraint(poly1, tvars)
      .showing(i"added to constraint: [$poly1] $params%, %\n$debugBoundsDescription", gadts)
  }

  override def addBound(path: PathType, sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean = {
    @annotation.tailrec def stripInternalTypeVar(tp: Type): Type = tp match {
      case tv: TypeVar =>
        val inst = constraint.instType(tv)
        if (inst.exists) stripInternalTypeVar(inst) else tv
      case _ => tp
    }

    val symTvar: TypeVar = stripInternalTypeVar(tvarOrError(path, sym)) match {
      case tv: TypeVar => tv
      case inst =>
        gadts.println(i"instantiated: $path.$sym -> $inst")
        return if (isUpper) isSub(inst, bound) else isSub(bound, inst)
    }

    val internalizedBound = bound match {
      case nt: TypeRef =>
        val ntTvar = tvarOf(nt)
        if (ntTvar != null) stripInternalTypeVar(ntTvar) else bound
      case _ => bound
    }

    val saved = constraint
    val result = internalizedBound match
      case boundTvar: TypeVar =>
        if (boundTvar eq symTvar) true
        else if (isUpper) addLess(symTvar.origin, boundTvar.origin)
        else addLess(boundTvar.origin, symTvar.origin)
      case bound =>
        addBoundTransitively(symTvar.origin, bound, isUpper)

    gadts.println {
      val descr = if (isUpper) "upper" else "lower"
      val op = if (isUpper) "<:" else ">:"
      i"adding $descr bound $path.$sym $op $bound = $result"
    }

    if constraint ne saved then wasConstrained = true
    result
  }

  override def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean = {
    @annotation.tailrec def stripInternalTypeVar(tp: Type): Type = tp match {
      case tv: TypeVar =>
        val inst = constraint.instType(tv)
        if (inst.exists) stripInternalTypeVar(inst) else tv
      case _ => tp
    }

    val symTvar: TypeVar = stripInternalTypeVar(tvarOrError(sym)) match {
      case tv: TypeVar => tv
      case inst =>
        gadts.println(i"instantiated: $sym -> $inst")
        return if (isUpper) isSub(inst, bound) else isSub(bound, inst)
    }

    val internalizedBound = bound match {
      case nt: TypeRef =>
        val ntTvar = tvarOf(nt)
        if (ntTvar != null) stripInternalTypeVar(ntTvar) else bound
      case _ => bound
    }

    val saved = constraint
    val result = internalizedBound match
      case boundTvar: TypeVar =>
        if (boundTvar eq symTvar) true
        else if (isUpper) addLess(symTvar.origin, boundTvar.origin)
        else addLess(boundTvar.origin, symTvar.origin)
      case bound =>
        addBoundTransitively(symTvar.origin, bound, isUpper)

    gadts.println {
      val descr = if (isUpper) "upper" else "lower"
      val op = if (isUpper) "<:" else ">:"
      i"adding $descr bound $sym $op $bound = $result"
    }

    if constraint ne saved then wasConstrained = true
    result
  }

  private def lookupPath(p: PathType): PathType | Null =
    def recur(p: PathType): PathType | Null = myUnionFind(p) match
      case null => null
      case q: PathType if q eq p => q
      case q: PathType =>
        recur(q)

    recur(p)

  override def addEquality(p: PathType, q: PathType)(using Context): Unit =
    val pRep: PathType | Null = lookupPath(p)
    val qRep: PathType | Null = lookupPath(q)

    val newRep = (pRep, qRep) match
      case (null, null) => p
      case (null, r: PathType) => r
      case (r: PathType, null) => r
      case (r1: PathType, r2: PathType) =>
        myUnionFind = myUnionFind.updated(r2, r1)
        r1

    myUnionFind = myUnionFind.updated(p, newRep)
    myUnionFind = myUnionFind.updated(q, newRep)

  override def isEquivalent(p: PathType, q: PathType): Boolean =
    lookupPath(p) match
      case null => false
      case p0: PathType => lookupPath(q) match
        case null => false
        case q0: PathType => p0 eq q0

  override def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean =
    constraint.isLess(tvarOrError(sym1).origin, tvarOrError(sym2).origin)

  override def isLess(tp1: NamedType, tp2: NamedType)(using Context): Boolean =
    constraint.isLess(tvarOrError(tp1).origin, tvarOrError(tp2).origin)

  override def fullBounds(sym: Symbol)(using Context): TypeBounds | Null =
    mapping(sym) match {
      case null => null
      // TODO: Improve flow typing so that ascription becomes redundant
      case tv: TypeVar =>
        fullBounds(tv.origin)
          // .ensuring(containsNoInternalTypes(_))
    }

  override def fullBounds(p: PathType, sym: Symbol)(using Context): TypeBounds | Null =
    tvarOf(p, sym) match {
      case null => null
      case tv => fullBounds(tv.nn.origin)
    }

  override def fullBounds(tp: TypeRef)(using Context): TypeBounds | Null =
    tp match {
      case TypeRef(p: PathType, _) => fullBounds(p, tp.symbol)
      case _ => null
    }

  override def bounds(sym: Symbol)(using Context): TypeBounds | Null =
    mapping(sym) match {
      case null => null
      // TODO: Improve flow typing so that ascription becomes redundant
      case tv: TypeVar =>
        def retrieveBounds: TypeBounds = externalize(bounds(tv.origin)).bounds
        retrieveBounds
          //.showing(i"gadt bounds $sym: $result", gadts)
          //.ensuring(containsNoInternalTypes(_))
    }

  override def bounds(path: PathType, sym: Symbol)(using Context): TypeBounds | Null =
    tvarOf(path, sym) match {
      case null => null
      case tv: TypeVar =>
        def retrieveBounds: TypeBounds =
          bounds(tv.origin) match {
            case TypeAlias(tpr: TypeParamRef) if reverseMapping.contains(tpr) =>
              TypeAlias(reverseMapping(tpr).nn.typeRef)
            case TypeAlias(tpr: TypeParamRef) if pathDepReverseMapping.contains(tpr) =>
              TypeAlias(pathDepReverseMapping(tpr).nn)
            case tb => tb
          }
        retrieveBounds
    }

  override def bounds(tp: TypeRef)(using Context): TypeBounds | Null =
    tp match {
      case TypeRef(p: PathType, _) => bounds(p, tp.symbol)
      case _ => null
    }

  override def contains(sym: Symbol)(using Context): Boolean = mapping(sym) != null

  override def contains(path: PathType)(using Context): Boolean = pathDepMapping(path) != null

  override def contains(path: PathType, sym: Symbol)(using Context): Boolean = pathDepMapping(path) match
    case null => false
    case innerMapping => innerMapping.nn(sym) != null

  override def registeredTypeMembers(path: PathType): List[Symbol] = pathDepMapping(path).nn.keys

  def isNarrowing: Boolean = wasConstrained

  override def approximation(sym: Symbol, fromBelow: Boolean, maxLevel: Int)(using Context): Type = {
    val res =
      approximation(tvarOrError(sym).origin, fromBelow, maxLevel) match
        case tpr: TypeParamRef =>
          // Here we do externalization when the returned type is a TypeParamRef,
          //  b/c ConstraintHandling.approximation may return internal types when
          //  the type variable is instantiated. See #15531.
          externalize(tpr)
        case tp => tp

    gadts.println(i"approximating $sym ~> $res")
    res
  }

  override def symbols: List[Symbol] = mapping.keys

  override def fresh: GadtConstraint = new ProperGadtConstraint(
    myConstraint,
    mapping,
    reverseMapping,
    pathDepMapping,
    pathDepReverseMapping,
    wasConstrained,
    myScrutineePath,
    myUnionFind,
    myPatternSkolem,
  )

  def restore(other: GadtConstraint): Unit = other match {
    case other: ProperGadtConstraint =>
      this.myConstraint = other.myConstraint
      this.mapping = other.mapping
      this.reverseMapping = other.reverseMapping
      this.pathDepMapping = other.pathDepMapping
      this.pathDepReverseMapping = other.pathDepReverseMapping
      this.wasConstrained = other.wasConstrained
      this.myScrutineePath = other.myScrutineePath
      this.myUnionFind = other.myUnionFind
      this.myPatternSkolem = other.myPatternSkolem
    case _ => ;
  }

  override def scrutineePath: TermRef | Null = myScrutineePath

  override def resetScrutineePath(): Unit = myScrutineePath = null

  override def supplyPatternPath(path: TermRef)(using Context): Unit =
    if myPatternSkolem eq null then
      ()
    else
      def updateMappings() =
        pathDepMapping(myPatternSkolem.nn) match {
          case null =>
          case m: SimpleIdentityMap[Symbol, TypeVar] =>
            pathDepMapping = pathDepMapping.updated(path, m)
            m foreachBinding { (sym, tvar) =>
              val tpr = tvar.origin
              pathDepReverseMapping = pathDepReverseMapping.updated(tpr, TypeRef(path, sym))
            }
        }

      def updateUnionFind() =
        myUnionFind(myPatternSkolem.nn) match {
          case null =>
          case repr: PathType =>
            myUnionFind = myUnionFind.updated(path, repr)
        }

      updateMappings()
      updateUnionFind()
  end supplyPatternPath

  override def createPatternSkolem(pat: Type): SkolemType =
    myPatternSkolem = SkolemType(pat)
    myPatternSkolem.nn
  end createPatternSkolem

  override def withScrutineePath[T](path: TermRef | Null)(op: => T): T =
    val saved = this.myScrutineePath
    this.myScrutineePath = path

    val result = op

    this.myScrutineePath = saved
    result

  // ---- Protected/internal -----------------------------------------------

  override protected def constraint = myConstraint
  override protected def constraint_=(c: Constraint) = myConstraint = c

  override protected def isSub(tp1: Type, tp2: Type)(using Context): Boolean = TypeComparer.isSubType(tp1, tp2)
  override protected def isSame(tp1: Type, tp2: Type)(using Context): Boolean = TypeComparer.isSameType(tp1, tp2)

  override def nonParamBounds(param: TypeParamRef)(using Context): TypeBounds =
    externalize(constraint.nonParamBounds(param)).bounds

  override def fullLowerBound(param: TypeParamRef)(using Context): Type =
    constraint.minLower(param).foldLeft(nonParamBounds(param).lo) {
      (t, u) => t | externalize(u)
    }

  override def fullUpperBound(param: TypeParamRef)(using Context): Type =
    constraint.minUpper(param).foldLeft(nonParamBounds(param).hi) { (t, u) =>
      val eu = externalize(u)
      // Any as the upper bound means "no bound", but if F is higher-kinded,
      // Any & F = F[_]; this is wrong for us so we need to short-circuit
      if t.isAny then eu else t & eu
    }

  // ---- Private ----------------------------------------------------------

  /** Externalize the internal TypeParamRefs in the given type.
    *
    * We declare the method as `protected` instead of `private`, because declaring it as
    * private will break a pickling test. This is a temporary workaround.
    */
  protected def externalize(tp: Type, theMap: TypeMap | Null = null)(using Context): Type = tp match
    case param: TypeParamRef => reverseMapping(param) match
      case sym: Symbol => sym.typeRef
      case null        =>
        pathDepReverseMapping(param) match
          case tp: TypeRef => tp
          case null        => param
    case tp: TypeAlias       => tp.derivedAlias(externalize(tp.alias, theMap))
    case tp                  => (if theMap == null then ExternalizeMap() else theMap).mapOver(tp)

  private class ExternalizeMap(using Context) extends TypeMap:
    def apply(tp: Type): Type = externalize(tp, this)(using mapCtx)


  private def tvarOrError(sym: Symbol)(using Context): TypeVar =
    mapping(sym).ensuring(_ != null, i"not a constrainable symbol: $sym").uncheckedNN

  private def tvarOrError(path: PathType, sym: Symbol)(using Context): TypeVar =
    tvarOf(path, sym).ensuring(_ != null, i"not a constrainable type: $path.$sym").uncheckedNN

  private def tvarOrError(ntp: NamedType)(using Context): TypeVar =
    tvarOf(ntp).ensuring(_ != null, i"not a constrainable type: $ntp").uncheckedNN

  private def containsNoInternalTypes(tp: Type, theAcc: TypeAccumulator[Boolean] | Null = null)(using Context): Boolean = tp match {
    case tpr: TypeParamRef => !reverseMapping.contains(tpr)
    case tv: TypeVar => !reverseMapping.contains(tv.origin)
    case tp =>
      (if (theAcc != null) theAcc else new ContainsNoInternalTypesAccumulator()).foldOver(true, tp)
  }

  private class ContainsNoInternalTypesAccumulator(using Context) extends TypeAccumulator[Boolean] {
    override def apply(x: Boolean, tp: Type): Boolean = x && containsNoInternalTypes(tp, this)
  }

  // ---- Debug ------------------------------------------------------------

  override def constr = gadtsConstr

  override def toText(printer: Printer): Texts.Text = constraint.toText(printer)

  override def debugBoundsDescription(using Context): String = {
    val sb = new mutable.StringBuilder
    sb ++= constraint.show
    sb ++= "\nType parameter bounds:\n"
    mapping.foreachBinding { case (sym, _) =>
      sb ++= i"$sym: ${fullBounds(sym)}\n"
    }
    sb ++= "\nPath-dependent type bounds:\n"
    pathDepMapping foreachBinding { case (path, m) =>
      m foreachBinding { case (sym, _) =>
        sb ++= i"$path.$sym: ${fullBounds(TypeRef(path, sym))}\n"
      }
    }
    sb ++= "\nSingleton equalities:\n"
    myUnionFind foreachBinding { case (path, _) =>
      val repr = lookupPath(path)
      repr match
        case repr: PathType if repr ne path =>
          sb ++= i"$path.type: $repr.type\n"
        case _ =>
    }

    sb.result
  }
}

@sharable object EmptyGadtConstraint extends GadtConstraint {
  override def bounds(sym: Symbol)(using Context): TypeBounds | Null = null
  override def fullBounds(sym: Symbol)(using Context): TypeBounds | Null = null

  override def bounds(p: PathType, sym: Symbol)(using Context): TypeBounds | Null = null
  override def fullBounds(p: PathType, sym: Symbol)(using Context): TypeBounds | Null = null
  override def bounds(tp: TypeRef)(using Context): TypeBounds | Null = null
  override def fullBounds(tp: TypeRef)(using Context): TypeBounds | Null = null

  override def reprOf(p: PathType): PathType | Null = null

  override def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean = unsupported("EmptyGadtConstraint.isLess")
  override def isLess(tp1: NamedType, tp2: NamedType)(using Context): Boolean = unsupported("EmptyGadtConstraint.isLess")

  override def isNarrowing: Boolean = false

  override def contains(sym: Symbol)(using Context) = false

  override def contains(path: PathType)(using Context) = false

  override def contains(path: PathType, symbol: Symbol)(using Context) = false

  override def isConstrainablePDT(path: PathType, symbol: Symbol)(using Context) = false

  override def registeredTypeMembers(path: PathType): List[Symbol] = Nil

  override def addToConstraint(params: List[Symbol])(using Context): Boolean = unsupported("EmptyGadtConstraint.addToConstraint")
  override def addToConstraint(path: PathType)(using Context): Boolean = false
  override def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean = unsupported("EmptyGadtConstraint.addBound")
  override def addBound(path: PathType, sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean = unsupported("EmptyGadtConstraint.addBound")

  override def addEquality(p: PathType, q: PathType)(using Context) = ()

  override def isEquivalent(p: PathType, q: PathType) = false

  override def approximation(sym: Symbol, fromBelow: Boolean)(using Context): Type = unsupported("EmptyGadtConstraint.approximation")

  override def symbols: List[Symbol] = Nil

  override def scrutineePath: TermRef | Null = null

  override def resetScrutineePath(): Unit = ()

  override def withScrutineePath[T](path: TermRef | Null)(op: => T): T = op

  override def supplyPatternPath(path: TermRef)(using Context): Unit = ()

  override def createPatternSkolem(pat: Type): SkolemType = unsupported("EmptyGadtConstraint.createPatternSkolem")

  override def fresh = new ProperGadtConstraint
  override def restore(other: GadtConstraint): Unit =
    assert(!other.isNarrowing, "cannot restore a non-empty GADTMap")

  override def debugBoundsDescription(using Context): String = "EmptyGadtConstraint"

  override def toText(printer: Printer): Texts.Text = "EmptyGadtConstraint"
}
