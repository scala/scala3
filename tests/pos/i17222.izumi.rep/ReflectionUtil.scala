package izumi.reflect.dottyreflection

import scala.annotation.{tailrec, unused}
import scala.collection.immutable.Queue
import scala.quoted.Quotes

private[dottyreflection] trait ReflectionUtil { this: InspectorBase =>

  import qctx.reflect.*

  private final lazy val ignoredInIntersections0: Set[TypeRepr] = {
    Set(
      defn.AnyClass.typeRef,
      defn.MatchableClass.typeRef,
      defn.AnyRefClass.typeRef,
      defn.ObjectClass.typeRef
    )
  }
  def ignoredInIntersections(repr: qctx.reflect.TypeRepr): Boolean = {
    ignoredInIntersections0.exists(_ =:= repr)
  }
  def ignoredInUnions(repr: qctx.reflect.TypeRepr): Boolean = {
    repr =:= defn.NothingClass.typeRef
  }

  protected final def flattenAnd(tpe: TypeRepr): List[TypeRepr] =
    tpe.dealias match {
      case AndType(lhs, rhs) => flattenAnd(lhs) ++ flattenAnd(rhs)
      case _ => List(tpe)
    }

  protected final def flattenOr(tpe: TypeRepr): List[TypeRepr] =
    tpe.dealias match {
      case OrType(lhs, rhs) => flattenOr(lhs) ++ flattenOr(rhs)
      case _ => List(tpe)
    }

  protected final def intersectionUnionRefinementClassPartsOf(tpe: TypeRepr): List[TypeRepr] = {
    tpe.dealias match {
      case AndType(lhs, rhs) =>
        intersectionUnionRefinementClassPartsOf(lhs) ++ intersectionUnionRefinementClassPartsOf(rhs)
      case OrType(lhs, rhs) =>
        intersectionUnionRefinementClassPartsOf(lhs) ++ intersectionUnionRefinementClassPartsOf(rhs)
      case refinement: Refinement =>
        intersectionUnionRefinementClassPartsOf(refinement.parent)
      case _ =>
        List(tpe)
    }
  }

  protected final def refinementInfoToParts(tpe0: TypeRepr): List[TypeRepr] = {
    tpe0 match {
      case ByNameType(tpe) =>
        refinementInfoToParts(tpe)
      case MethodType(_, args, res) =>
        args.flatMap(refinementInfoToParts) ++ refinementInfoToParts(res)
      case PolyType(_, tbounds, res) =>
        // FIXME we need to do FullDbInspector.inspectTypeReprToFullBases.lambdify/LightTypeTagImpl.makeLambdaOnlyBases.makeLambdaParents
        //   to wrap the unresolved type params in `res` into a lambda.
        //   As is, if type parameters are used in `res`, we'll add lots of trash types into db
        tbounds.flatMap { case TypeBounds(lo, hi) => List(lo, hi) } ++ refinementInfoToParts(res)
      case tpe =>
        List(tpe)
    }
  }

  protected final def flattenRefinements(ref: Refinement): (Queue[(Symbol, String, TypeRepr)], TypeRepr) = {
    val refinementDecl = (ref.typeSymbol, ref.name, ref.info)
    ref.parent match {
      case innerRefinement: Refinement =>
        val (innerRefs, nonRefinementParent) = flattenRefinements(innerRefinement)
        (innerRefs :+ refinementDecl, nonRefinementParent)
      case nonRefinementParent =>
        (Queue(refinementDecl), nonRefinementParent)
    }
  }

  protected final def allPartsStrong(outerOwnerClassDefs: Set[Symbol], typeRepr: TypeRepr): Boolean = {
    ReflectionUtil.allPartsStrong(using qctx)(shift, outerOwnerClassDefs, Set.empty, typeRepr)
  }

  protected final def getClassDefOwners(symbol: Symbol): Set[Symbol] = {
    ReflectionUtil.getClassDefOwners(using qctx)(symbol)
  }

  import ReflectionUtil.reflectiveUncheckedNonOverloadedSelectable
  import InternalContext.InternalContext

  extension (typeRef: TypeRef | ParamRef) {
    protected final def _underlying: TypeRepr = {
      // This works as a substitution for `TypeRef#underlying` call,
      // but I'm not sure if it's a reliable substitution.

//      typeRef.typeSymbol.owner._typeRef.memberType(typeRef.typeSymbol)

      // No, It's not a reliable substitution. When used on a TypeParamRef it returns Any instead of the underlying TypeBounds
      // https://github.com/lampepfl/dotty/issues/15799

//      val underlying = typeRef
//        .getClass.getMethods.collect { case m if m.getName == "underlying" => m }.head.invoke(
//          typeRef,
//          qctx.getClass.getMethods.collect { case m if m.getName == "ctx" => m }.head.invoke(qctx)
//        )
//      underlying.asInstanceOf[TypeRepr]

      typeRef.asInstanceOf[InternalTypeRefOrParamRef].underlying(qctx._ctx)
    }
  }

  extension (typeRepr: TypeRepr) {
    protected final def _paramVariancesIfHKTypeLambda: Option[List[Flags]] = {
      try {
        val params = typeRepr.asInstanceOf[InternalHKTypeLambda].typeParams
        val flags = params.map(_.paramVariance(qctx._ctx))
        Some(flags)
      } catch {
        case _: NoSuchMethodException => None
      }
    }

    @tailrec
    protected final def _dealiasSimplifiedFull: TypeRepr = {
//      val res = typeRepr.dealias.simplified
      // simplified does everything below functions do, with exception of `_removeTautologicalUnions` for some reason
      // All of these would be more useful, if not for forced type simplification on implicit macro - https://github.com/lampepfl/dotty/issues/17544
      val res = typeRepr.dealias._removeTautologicalIntersections._removeTautologicalUnions._simplifyMatchCase
      if (res.asInstanceOf[AnyRef] eq typeRepr.asInstanceOf[AnyRef]) {
        res
      } else {
        res._dealiasSimplifiedFull
      }
    }

    // Calling .simplified will remove too many intersections - we only want to remove those with Any/AnyRef/Object/Matchable
    @tailrec private def _removeTautologicalIntersections: TypeRepr = {
      typeRepr match {
        case AndType(a, b) =>
          if (ignoredInIntersections(a)) {
            b._removeTautologicalIntersections
          } else if (ignoredInIntersections(b)) {
            a._removeTautologicalIntersections
          } else {
            removeTautologicalIntersectionsNonTailRec(a, b)
          }
        case _ =>
          typeRepr
      }
    }

    private def removeTautologicalIntersectionsNonTailRec(a: TypeRepr, b: TypeRepr): TypeRepr = {
      val a0 = a._removeTautologicalIntersections
      val b0 = b._removeTautologicalIntersections
      if ((a.asInstanceOf[AnyRef] ne a0.asInstanceOf[AnyRef]) || (b.asInstanceOf[AnyRef] ne b0.asInstanceOf[AnyRef])) {
        AndType(a0, b0)
      } else {
        typeRepr
      }
    }

    @tailrec private def _removeTautologicalUnions: TypeRepr = {
      typeRepr match {
        case OrType(a, b) =>
          if (ignoredInUnions(a)) {
            b._removeTautologicalUnions
          } else if (ignoredInUnions(b)) {
            a._removeTautologicalUnions
          } else {
            removeTautologicaUnionsNonTailRec(a, b)
          }
        case _ =>
          typeRepr
      }
    }

    private def removeTautologicaUnionsNonTailRec(a: TypeRepr, b: TypeRepr): TypeRepr = {
      val superA = ignoredInIntersections(a)
      val superB = ignoredInIntersections(b)
      if (superA && superB) {
        (if (a <:< b) b else a)._removeTautologicalUnions
      } else if (superA) {
        a
      } else if (superB) {
        b
      } else {
        val a0 = a._removeTautologicalUnions
        val b0 = b._removeTautologicalUnions
        if ((a.asInstanceOf[AnyRef] ne a0.asInstanceOf[AnyRef]) || (b.asInstanceOf[AnyRef] ne b0.asInstanceOf[AnyRef])) {
          AndType(a0, b0)
        } else {
          typeRepr
        }
      }
    }

    inline private def _simplifyMatchCase: TypeRepr = {
      typeRepr match {
        case _: MatchCase | _: MatchType =>
          // no other way to evaluate a match type other than calling simplified,
          // even though that'll also cause a collapse of tautological intersections
          // other than with Any/AnyRef/Object/Matchable
          typeRepr.simplified
        case _ =>
          typeRepr
      }
    }

  }

  extension (qctx: Quotes) {
    final def _ctx: InternalContext = qctx.asInstanceOf[{ def ctx: InternalContext }].ctx
  }

  type InternalTypeRefOrParamRef = {
    def underlying(ctx: InternalContext): TypeRepr
  }

  type InternalHKTypeLambda = {
    val typeParams: List[InternalLambdaParam]
  }

  type InternalLambdaParam = {
    def paramVariance(ctx: InternalContext): Flags
  }

  object InternalContext {
    opaque type InternalContext = Any
  }

}

private[reflect] object ReflectionUtil {

  private[reflect] inline implicit def reflectiveUncheckedNonOverloadedSelectable(x: Any): UncheckedNonOverloadedSelectable = new UncheckedNonOverloadedSelectable(x)

  /**
    * Returns true if the given type contains no type parameters
    * (this means the type is not "weak" https://stackoverflow.com/questions/29435985/weaktypetag-v-typetag)
    */
  private[reflect] def allPartsStrong(
    using qctx: Quotes
  )(shift: Int,
    outerOwnerClassDefs: Set[qctx.reflect.Symbol],
    outerLambdas: Set[qctx.reflect.TypeRepr],
    typeRepr: qctx.reflect.TypeRepr
  ): Boolean = {
    import qctx.reflect.*
    typeRepr.dealias match {
      case x if topLevelWeakType(outerOwnerClassDefs, outerLambdas, x) => false
      case AppliedType(tpe, args) =>
        allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, tpe) && args.forall(allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, _))
      case AndType(lhs, rhs) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, lhs) && allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, rhs)
      case OrType(lhs, rhs) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, lhs) && allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, rhs)
      case TypeRef(tpe, _) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, tpe)
      case TermRef(tpe, _) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, tpe)
      case ThisType(tpe) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, tpe)
      case NoPrefix() => true
      case TypeBounds(lo, hi) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, lo) && allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, hi)
      case lam @ TypeLambda(_, _, body) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas + lam, body)
      case Refinement(parent, _, tpe) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, tpe) && allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, parent)
      case ByNameType(tpe) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, tpe)
      case strange =>
        InspectorBase.log(shift, s"Got unknown type component when checking strength: $strange")
        true
    }
  }

  private[reflect] def topLevelWeakType(
    using qctx: Quotes
  )(outerOwnerClassDefs: Set[qctx.reflect.Symbol],
    outerLambdas: Set[qctx.reflect.TypeRepr],
    typeRepr: qctx.reflect.TypeRepr
  ): Boolean = {
    import qctx.reflect.*
    typeRepr match {
      case x if x.typeSymbol.isTypeParam =>
        x match {
          case t: ParamRef if outerLambdas.contains(t.binder) => false
          case _ => true
        }
      // we regard abstract types like T in trait X { type T; Tag[this.T] } - when we are _inside_ the definition template
      // as 'type parameters' too. So that you could define `implicit def tagForT: Tag[this.T]` and the tag would be resolved
      // to this implicit correctly, instead of generating a useless `X::this.type::T` tag.
      // TODO: Due to https://github.com/lampepfl/dotty/issues/16107 not being fixed we have to make sure we're actually
      //       inside the definition of the this-type prefix to count it as 'weak' - unlike Scala 2 we're not protected
      //       from this-types leaking in and have to carry the owner chain here - until that issue is fixed.
      case x @ TypeRef(ThisType(prefix), _) if x.typeSymbol.isAbstractType && !x.typeSymbol.isClassDef && outerOwnerClassDefs.contains(prefix.typeSymbol) =>
        true
      case _ => false
    }
  }

  private[reflect] def getClassDefOwners(using qctx: Quotes)(symbol: qctx.reflect.Symbol): Set[qctx.reflect.Symbol] = {
    Iterator
      .iterate(symbol) {
        s =>
          val owner = s.owner
          if (owner == null || owner.isNoSymbol || owner == qctx.reflect.defn.RootClass) {
            null.asInstanceOf[qctx.reflect.Symbol]
          } else {
            owner
          }
      }
      .takeWhile(_ ne null)
      .filter(s => s.isClassDef && !s.isAbstractType)
      .toSet
  }

  private[reflect] final class UncheckedNonOverloadedSelectable(private val selectable: Any) extends AnyVal with Selectable {

    inline def selectDynamic(name: String): Any = {
      applyDynamic(name)()
    }

    def applyDynamic(name: String, @unused paramTypes: Class[_]*)(args: Any*): Any = {
      val cls = selectable.getClass
      val method = {
        if (args.isEmpty) {
          cls.getMethod(name)
        } else {
          cls.getMethods.collectFirst { case m if m.getName == name => m } match {
            case Some(m) => m
            case None => throw new NoSuchMethodException(s"No method named `$name` found in class `$cls`")
          }
        }
      }
      method.invoke(selectable, args*)
    }

  }

}
