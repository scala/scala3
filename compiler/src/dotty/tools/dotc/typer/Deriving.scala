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
trait Deriving { this: Typer =>

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
      else {
        // If we set the Synthetic flag here widenImplied will widen too far and the
        // derived instance will have too low a priority to be selected over a freshly
        // derived instance at the summoning site.
        synthetics +=
          ctx.newSymbol(ctx.owner, instanceName, Implied | Method, info, coord = pos.span)
            .entered
      }
    }

    /** Check derived type tree `derived` for the following well-formedness conditions:
     *  (1) It must be a class type with a stable prefix (@see checkClassTypeWithStablePrefix)
     *  (2) It must have exactly one type parameter
     *  If it passes the checks, enter a typeclass instance for it in the current scope.
     *  Given
     *
     *     class C[Ts] .... derives ... D ...
     *
     *  where `T_1, ..., T_n` are the first-kinded type parameters in `Ts`,
     *  the typeclass instance has the form
     *
     *      implicit def derived$D(implicit ev_1: D[T_1], ..., ev_n: D[T_n]): D[C[Ts]] = D.derived
     *
     *  See the body of this method for how to generalize this to typeclasses with more
     *  or less than one type parameter.
     *
     *  See test run/typeclass-derivation2 and run/derive-multi
     *  for examples that spell out what would be generated.
     *
     *  Note that the name of the derived method contains the name in the derives clause, not
     *  the underlying class name. This allows one to disambiguate derivations of type classes
     *  that have the same name but different prefixes through selective aliasing.
     */
    private def processDerivedInstance(derived: untpd.Tree): Unit = {
      val originalType = typedAheadType(derived, AnyTypeConstructorProto).tpe
      val underlyingType = underlyingClassRef(originalType)
      val derivedType = checkClassType(underlyingType, derived.sourcePos, traitReq = false, stablePrefixReq = true)
      val typeClass = derivedType.classSymbol
      val nparams = typeClass.typeParams.length

      lazy val clsTpe = cls.typeRef.EtaExpand(cls.typeParams)
      if (nparams == 1 && clsTpe.hasSameKindAs(typeClass.typeParams.head.info)) {
        // A "natural" type class instance ... the kind of the data type
        // matches the kind of the unique type class type parameter

        val resultType = derivedType.appliedTo(clsTpe)
        val instanceInfo = ExprType(resultType)
        addDerivedInstance(originalType.typeSymbol.name, instanceInfo, derived.sourcePos)
      } else {
        // A matrix of all parameter combinations of current class parameters
        // and derived typeclass parameters.
        // Rows: parameters of current class
        // Columns: parameters of typeclass

        // Running example: typeclass: class TC[X, Y, Z], deriving class: class A[T, U]
        // clsParamss =
        //     T_X  T_Y  T_Z
        //     U_X  U_Y  U_Z
        val clsParamss: List[List[TypeSymbol]] = cls.typeParams.map { tparam =>
          if (nparams == 0) Nil
          else if (nparams == 1) tparam :: Nil
          else typeClass.typeParams.map(tcparam =>
            tparam.copy(name = s"${tparam.name}_$$_${tcparam.name}".toTypeName)
              .asInstanceOf[TypeSymbol])
        }
        val firstKindedParamss = clsParamss.filter {
          case param :: _ => !param.info.isLambdaSub
          case nil => false
        }

        // The types of the required evidence parameters. In the running example:
        // TC[T_X, T_Y, T_Z], TC[U_X, U_Y, U_Z]
        val evidenceParamInfos =
          for (row <- firstKindedParamss)
          yield derivedType.appliedTo(row.map(_.typeRef))

        // The class instances in the result type. Running example:
        //   A[T_X, U_X], A[T_Y, U_Y], A[T_Z, U_Z]
        val resultInstances =
          for (n <- List.range(0, nparams))
          yield cls.typeRef.appliedTo(clsParamss.map(row => row(n).typeRef))

        // TC[A[T_X, U_X], A[T_Y, U_Y], A[T_Z, U_Z]]
        val resultType = derivedType.appliedTo(resultInstances)

        val clsParams: List[TypeSymbol] = clsParamss.flatten
        val instanceInfo =
          if (clsParams.isEmpty) ExprType(resultType)
          else PolyType.fromParams(clsParams, ImplicitMethodType(evidenceParamInfos, resultType))
        addDerivedInstance(originalType.typeSymbol.name, instanceInfo, derived.sourcePos)
      }
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
      def typeclassInstance(sym: Symbol)(implicit ctx: Context): List[Type] => (List[List[tpd.Tree]] => tpd.Tree) =
        (tparamRefs: List[Type]) => (paramRefss: List[List[tpd.Tree]]) => {
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
