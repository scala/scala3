package dotty.tools
package dotc
package typer

import core._
import ast._
import ast.Trees._
import StdNames._
import Contexts._, Symbols._, Types._, SymDenotations._, Names._, NameOps._, Flags._, Decorators._
import ProtoTypes._
import util.Spans._
import util.SourcePosition
import collection.mutable
import Constants.Constant
import config.Printers.derive
import Inferencing._
import transform.TypeUtils._
import transform.SymUtils._
import ErrorReporting.errorTree

/** A typer mixin that implements typeclass derivation functionality */
trait Deriving {
  this: Typer =>

  /** A helper class to derive type class instances for one class or object
   *  @param  cls      The class symbol of the class or object with a `derives` clause
   *  @param  codePos  The default position that should be given to generic
   *                   synthesized infrastructure code that is not connected with a
   *                   `derives` instance.
   */
  class Deriver(cls: ClassSymbol, codePos: SourcePosition)(implicit ctx: Context) {

    /** A buffer for synthesized symbols for type class instances */
    private var synthetics = new mutable.ListBuffer[Symbol]

    /** A version of Type#underlyingClassRef that works also for higher-kinded types */
    private def underlyingClassRef(tp: Type): Type = tp match {
      case tp: TypeRef if tp.symbol.isClass => tp
      case tp: TypeRef if tp.symbol.isAbstractType => NoType
      case tp: TermRef => NoType
      case tp: TypeProxy => underlyingClassRef(tp.underlying)
      case _ => NoType
    }

    /** Enter type class instance with given name and info in current scope, provided
     *  an instance with the same name does not exist already.
     *  @param  reportErrors  Report an error if an instance with the same name exists already
     */
    private def addDerivedInstance(clsName: Name, info: Type, pos: SourcePosition): Unit = {
      val instanceName = s"derived$$$clsName".toTermName
      if (ctx.denotNamed(instanceName).exists)
        ctx.error(i"duplicate typeclass derivation for $clsName", pos)
      else
        // If we set the Synthetic flag here widenGiven will widen too far and the
        // derived instance will have too low a priority to be selected over a freshly
        // derived instance at the summoning site.
        synthetics +=
          ctx.newSymbol(ctx.owner, instanceName, Given | Method, info, coord = pos.span)
            .entered
    }

    /** Check derived type tree `derived` for the following well-formedness conditions:
     *  (1) It must be a class type with a stable prefix (@see checkClassTypeWithStablePrefix)
     *
     *  (2) It must belong to one of the following three categories:
     *      (a) a single paramter type class with a parameter which matches the kind of
     *          the deriving ADT
     *      (b) a single parameter type class with a parameter of kind * and an ADT with
     *          one or more type parameter of kind *
     *      (c) the Eql type class
     *
     *      See detailed descriptions in deriveSingleParameter and deriveEql below.
     *
     *  If it passes the checks, enter a typeclass instance for it in the current scope.
     *
     *  See test run/typeclass-derivation2, run/poly-kinded-derives and pos/derive-eq
     *  for examples that spell out what would be generated.
     *
     *  Note that the name of the derived method contains the name in the derives clause, not
     *  the underlying class name. This allows one to disambiguate derivations of type classes
     *  that have the same name but different prefixes through selective aliasing.
     */
    private def processDerivedInstance(derived: untpd.Tree): Unit = {
      val originalTypeClassType = typedAheadType(derived, AnyTypeConstructorProto).tpe
      val typeClassType = checkClassType(underlyingClassRef(originalTypeClassType), derived.sourcePos, traitReq = false, stablePrefixReq = true)
      val typeClass = typeClassType.classSymbol
      val typeClassParams = typeClass.typeParams
      val typeClassArity = typeClassParams.length

      def sameParamKinds(xs: List[ParamInfo], ys: List[ParamInfo]): Boolean =
        xs.corresponds(ys)((x, y) => x.paramInfo.hasSameKindAs(y.paramInfo))

      def cannotBeUnified =
        ctx.error(i"${cls.name} cannot be unified with the type argument of ${typeClass.name}", derived.sourcePos)

      def addInstance(derivedParams: List[TypeSymbol], evidenceParamInfos: List[List[Type]], instanceTypes: List[Type]): Unit = {
        val resultType = typeClassType.appliedTo(instanceTypes)
        val methodOrExpr =
          if (evidenceParamInfos.isEmpty) ExprType(resultType)
          else ImplicitMethodType(evidenceParamInfos.map(typeClassType.appliedTo), resultType)
        val derivedInfo = if (derivedParams.isEmpty) methodOrExpr else PolyType.fromParams(derivedParams, methodOrExpr)
        addDerivedInstance(originalTypeClassType.typeSymbol.name, derivedInfo, derived.sourcePos)
      }

      def deriveSingleParameter: Unit = {
        // Single parameter type classes ... (a) and (b) above
        //
        // (a) ADT and type class parameters overlap on the right and have the
        //     same kinds at the overlap.
        //
        //     Examples:
        //
        //     Type class: TC[F[T, U]]
        //
        //     ADT: C[A, B, C, D]         (C, D have same kinds as T, U)
        //
        //          given derived$TC[a, b]: TC[[t, u] =>> C[a, b, t, u]]
        //
        //     ADT: C[A, B, C]            (B, C have same kinds at T, U)
        //
        //          given derived$TC   [a]: TC[[t, u] =>>    C[a, t, u]]
        //
        //     ADT: C[A, B]               (A, B have same kinds at T, U)
        //
        //          given derived$TC      : TC[              C         ]  // a "natural" instance
        //
        //     ADT: C[A]                  (A has same kind as U)
        //
        //          given derived$TC      : TC[[t, u] =>>    C[      u]]
        //
        // (b) The type class and all ADT type parameters are of kind *
        //
        //     In this case the ADT has at least one type parameter of kind *,
        //     otherwise it would already have been covered as a "natural" case
        //     for a type class of the form F[_].
        //
        //     The derived instance has a type parameter and a given for
        //     each of the type parameters of the ADT,
        //
        //     Example:
        //
        //     Type class: TC[T]
        //
        //     ADT: C[A, B, C]
        //
        //          given derived$TC[a, b, c] given TC[a], TC[b], TC[c]: TC[a, b, c]
        //
        //     This, like the derivation for Eql, is a special case of the
        //     earlier more general multi-parameter type class model for which
        //     the heuristic is typically a good one.

        val typeClassParamType = typeClassParams.head.info
        val typeClassParamInfos = typeClassParamType.typeParams
        val instanceArity = typeClassParamInfos.length
        val clsType = cls.typeRef
        val clsParams = cls.typeParams
        val clsParamInfos = clsType.typeParams
        val clsArity = clsParamInfos.length
        val alignedClsParamInfos = clsParamInfos.takeRight(instanceArity)
        val alignedTypeClassParamInfos = typeClassParamInfos.take(alignedClsParamInfos.length)


        if ((instanceArity == clsArity || instanceArity > 0) && sameParamKinds(alignedClsParamInfos, alignedTypeClassParamInfos)) {
          // case (a) ... see description above
          val derivedParams = clsParams.dropRight(instanceArity)
          val instanceType =
            if (instanceArity == clsArity) clsType.EtaExpand(clsParams)
            else {
              val derivedParamTypes = derivedParams.map(_.typeRef)

              HKTypeLambda(typeClassParamInfos.map(_.paramName))(
                tl => typeClassParamInfos.map(_.paramInfo.bounds),
                tl => clsType.appliedTo(derivedParamTypes ++ tl.paramRefs.takeRight(clsArity)))
            }

          addInstance(derivedParams, Nil, List(instanceType))
        }
        else if (instanceArity == 0 && !clsParams.exists(_.info.isLambdaSub)) {
          // case (b) ... see description above
          val instanceType = clsType.appliedTo(clsParams.map(_.typeRef))
          val evidenceParamInfos = clsParams.map(param => List(param.typeRef))
          addInstance(clsParams, evidenceParamInfos, List(instanceType))
        }
        else
          cannotBeUnified
      }

      def deriveEql: Unit = {
        // Specific derives rules for the Eql type class ... (c) above
        //
        // This has been extracted from the earlier more general multi-parameter
        // type class model. Modulo the assumptions below, the implied semantics
        // are reasonable defaults.
        //
        // Assumptions:
        // 1. Type params of the deriving class correspond to all and only
        // elements of the deriving class which are relevant to equality (but:
        // type params could be phantom, or the deriving class might have an
        // element of a non-Eql type non-parametrically).
        //
        // 2. Type params of kinds other than * can be assumed to be irrelevant to
        // the derivation (but: eg. Foo[F[_]](fi: F[Int])).
        //
        // Are they reasonable? They cover some important cases (eg. Tuples of all
        // arities). derives Eql is opt-in, so if the semantics don't match those
        // appropriate for the deriving class the author of that class can provide
        // their own instance in the normal way. That being so, the question turns
        // on whether there are enough types which fit these semantics for the
        // feature to pay its way.

        // Procedure:
        // We construct a two column matrix of the deriving class type parameters
        // and the Eql typeclass parameters.
        //
        // Rows: parameters of the deriving class
        // Columns: parameters of the Eql typeclass (L/R)
        //
        // Running example: typeclass: class Eql[L, R], deriving class: class A[T, U, V]
        // clsParamss =
        //     T_L  T_R
        //     U_L  U_R
        //     V_L  V_R
        val clsParamss: List[List[TypeSymbol]] = cls.typeParams.map { tparam =>
          typeClassParams.map(tcparam =>
            tparam.copy(name = s"${tparam.name}_$$_${tcparam.name}".toTypeName)
              .asInstanceOf[TypeSymbol])
        }
        // Retain only rows with L/R params of kind * which Eql can be applied to.
        // No pairwise evidence will be required for params of other kinds.
        val firstKindedParamss = clsParamss.filter {
          case param :: _ => !param.info.isLambdaSub
          case _ => false
        }

        // The types of the required evidence parameters. In the running example:
        // Eql[T_L, T_R], Eql[U_L, U_R], Eql[V_L, V_R]
        val evidenceParamInfos =
          for (row <- firstKindedParamss)
          yield row.map(_.typeRef)

        // The class instances in the result type. Running example:
        //   A[T_L, U_L, V_L], A[T_R, U_R, V_R]
        val instanceTypes =
          for (n <- List.range(0, typeClassArity))
          yield cls.typeRef.appliedTo(clsParamss.map(row => row(n).typeRef))

        // Eql[A[T_L, U_L, V_L], A[T_R, U_R, V_R]]
        addInstance(clsParamss.flatten, evidenceParamInfos, instanceTypes)
      }

      if (typeClassArity == 1) deriveSingleParameter
      else if (typeClass == defn.EqlClass) deriveEql
      else if (typeClassArity == 0)
        ctx.error(i"type ${typeClass.name} in derives clause of ${cls.name} has no type parameters", derived.sourcePos)
      else
        cannotBeUnified
    }

    /** Create symbols for derived instances and infrastructure,
     *  append them to `synthetics` buffer, and enter them into class scope.
     *  Also, add generic instances if needed.
     */
    def enterDerived(derived: List[untpd.Tree]) =
      derived.foreach(processDerivedInstance(_))

    /** The synthesized type class instance definitions */
    def syntheticDefs: List[tpd.Tree] = {
      import tpd._

      /** The type class instance definition with symbol `sym` */
      def typeclassInstance(sym: Symbol)(implicit ctx: Context): List[Type] => (List[List[tpd.Tree]] => tpd.Tree) = {
        (tparamRefs: List[Type]) => (paramRefss: List[List[tpd.Tree]]) =>
          val tparams = tparamRefs.map(_.typeSymbol.asType)
          val params = if (paramRefss.isEmpty) Nil else paramRefss.head.map(_.symbol.asTerm)
          tparams.foreach(ctx.enter)
          params.foreach(ctx.enter)
          def instantiated(info: Type): Type = info match {
            case info: PolyType => instantiated(info.instantiate(tparamRefs))
            case info: MethodType => info.instantiate(params.map(_.termRef))
            case info => info.widenExpr
          }
          def companionRef(tp: Type): TermRef = tp match {
            case tp @ TypeRef(prefix, _) if tp.symbol.isClass =>
              prefix.select(tp.symbol.companionModule).asInstanceOf[TermRef]
            case tp: TypeProxy =>
              companionRef(tp.underlying)
          }
          val resultType = instantiated(sym.info)
          val module = untpd.ref(companionRef(resultType)).withSpan(sym.span)
          val rhs = untpd.Select(module, nme.derived)
          typed(rhs, resultType)
      }

      def syntheticDef(sym: Symbol): Tree =
        tpd.polyDefDef(sym.asTerm, typeclassInstance(sym)(ctx.fresh.setOwner(sym).setNewScope))

      synthetics.map(syntheticDef).toList
    }

    def finalize(stat: tpd.TypeDef): tpd.Tree = {
      val templ @ Template(_, _, _, _) = stat.rhs
      tpd.cpy.TypeDef(stat)(rhs = tpd.cpy.Template(templ)(body = templ.body ++ syntheticDefs))
    }
  }
}
