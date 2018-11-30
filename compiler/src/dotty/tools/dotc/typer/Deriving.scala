package dotty.tools
package dotc
package typer

import core._
import ast._
import ast.Trees._
import StdNames._
import Contexts._, Symbols._, Types._, SymDenotations._, Names._, NameOps._, Flags._, Decorators._
import NameKinds.DefaultGetterName
import ast.desugar, ast.desugar._
import ProtoTypes._
import util.Positions._
import util.Property
import collection.mutable
import tpd.ListOfTreeDecorator
import config.Config
import config.Printers.typr
import Annotations._
import Inferencing._
import transform.ValueClasses._
import transform.TypeUtils._
import transform.SymUtils._
import reporting.diagnostic.messages._

trait Deriving { this: Typer =>

  class Deriver(cls: ClassSymbol)(implicit ctx: Context) {

    private var synthetics = new mutable.ListBuffer[Symbol]

    private def caseShape(sym: Symbol): Type = {
      val (constr, elems) =
        sym match {
          case caseClass: ClassSymbol =>
            caseClass.primaryConstructor.info match {
              case info: PolyType =>
                def instantiate(implicit ctx: Context) = {
                  val poly = constrained(info, untpd.EmptyTree, alwaysAddTypeVars = true)._1
                  val mono @ MethodType(_) = poly.resultType
                  val resType = mono.finalResultType
                  resType <:< cls.appliedRef
                  val tparams = poly.paramRefs
                  val variances = caseClass.typeParams.map(_.paramVariance)
                  val instanceTypes = (tparams, variances).zipped.map((tparam, variance) =>
                    ctx.typeComparer.instanceType(tparam, fromBelow = variance < 0))
                  (resType.substParams(poly, instanceTypes),
                   mono.paramInfos.map(_.substParams(poly, instanceTypes)))
                }
                instantiate(ctx.fresh.setNewTyperState().setOwner(caseClass))
              case info: MethodType =>
                (cls.typeRef, info.paramInfos)
              case _ =>
                (cls.typeRef, Nil)
            }
          case _ =>
            (sym.termRef, Nil)
        }
      val elemShape = (elems :\ (defn.UnitType: Type))(defn.PairType.appliedTo(_, _))
      defn.ShapeCaseType.appliedTo(constr, elemShape)
    }

    lazy val children = cls.children.sortBy(_.pos.start)

    private def sealedShape: Type = {
      val cases = children.map(caseShape)
      val casesShape = (cases :\ (defn.UnitType: Type))(defn.PairType.appliedTo(_, _))
      defn.ShapeCasesType.appliedTo(casesShape)
    }

    lazy val shapeWithClassParams: Type =
      if (cls.is(Case)) caseShape(cls)
      else if (cls.is(Sealed)) sealedShape
      else NoType

    lazy val shape: Type = shapeWithClassParams match {
      case delayed: LazyRef => HKTypeLambda.fromParams(cls.typeParams, delayed.ref)
      case NoType => NoType
    }

    lazy val lazyShape: Type = shapeWithClassParams match {
      case delayed: LazyRef => HKTypeLambda.fromParams(cls.typeParams, delayed)
      case NoType => NoType
    }

    class ShapeCompleter extends TypeParamsCompleter {

      override def completerTypeParams(sym: Symbol)(implicit ctx: Context) = cls.typeParams

      def completeInCreationContext(denot: SymDenotation) = {
        val shape0 = shapeWithClassParams
        val tparams = cls.typeParams
        val abstractedShape =
          if (!shape0.exists) {
            ctx.error(em"Cannot derive for $cls; it is neither sealed nor a case class or object", cls.pos)
            UnspecifiedErrorType
          }
          else if (tparams.isEmpty)
            shape0
          else
            HKTypeLambda(tparams.map(_.name.withVariance(0)))(
              tl => tparams.map(tparam => tl.integrate(tparams, tparam.info).bounds),
              tl => tl.integrate(tparams, shape0))
        denot.info = TypeAlias(abstractedShape)
      }

      def complete(denot: SymDenotation)(implicit ctx: Context) =
        completeInCreationContext(denot)
    }

    private def add(sym: Symbol): sym.type = {
      ctx.enter(sym)
      synthetics += sym
      sym
    }

    /** Enter type class instance with given name and info in current scope, provided
      *  an instance woth the same name does not exist already.
      */
    private def addDerivedInstance(clsName: Name, info: Type, reportErrors: Boolean) = {
      val instanceName = s"derived$$$clsName".toTermName
      if (ctx.denotNamed(instanceName).exists) {
        if (reportErrors) ctx.error(i"duplicate typeclass derivation for $clsName")
      }
      else
        add(ctx.newSymbol(ctx.owner, instanceName, Synthetic | Method, info, coord = cls.pos))
    }

    /* Check derived type tree `derived` for the following well-formedness conditions:
      * (1) It must be a class type with a stable prefix (@see checkClassTypeWithStablePrefix)
      * (2) It must have exactly one type parameter
      * If it passes the checks, enter a typeclass instance for it in the current scope.
      */
    private def processDerivedInstance(derived: untpd.Tree): Unit = {
      val uncheckedType = typedAheadType(derived, AnyTypeConstructorProto).tpe.dealiasKeepAnnots
      val derivedType = checkClassType(uncheckedType, derived.pos, traitReq = false, stablePrefixReq = true)
      val nparams = derivedType.classSymbol.typeParams.length
      if (nparams == 1) {
        val typeClass = derivedType.classSymbol
        val firstKindedParams = cls.typeParams.filterNot(_.info.isLambdaSub)
        val evidenceParamInfos =
          for (param <- firstKindedParams) yield derivedType.appliedTo(param.typeRef)
        val resultType = derivedType.appliedTo(cls.appliedRef)
        val instanceInfo =
          if (evidenceParamInfos.isEmpty) ExprType(resultType)
          else PolyType.fromParams(firstKindedParams, ImplicitMethodType(evidenceParamInfos, resultType))
        addDerivedInstance(derivedType.typeSymbol.name, instanceInfo, reportErrors = true)
      }
      else
        ctx.error(
          i"derived class $derivedType should have one type paramater but has $nparams",
          derived.pos)
    }

    private def addShape(): Unit =
      if (!ctx.denotNamed(tpnme.Shape).exists) {
        val shapeSym = add(ctx.newSymbol(ctx.owner, tpnme.Shape, EmptyFlags, new ShapeCompleter))
        val shapedCls = defn.ShapedClass
        val lazyShapedInfo = new LazyType {
          def complete(denot: SymDenotation)(implicit ctx: Context) = {
            val tparams = cls.typeParams
            val shapedType = shapedCls.typeRef.appliedTo(
              cls.appliedRef,
              shapeSym.typeRef.appliedTo(tparams.map(_.typeRef)))
            denot.info = PolyType.fromParams(tparams, shapedType).ensureMethodic
          }
        }
        addDerivedInstance(shapedCls.name, lazyShapedInfo, reportErrors = false)
      }

    def enterDerived(derived: List[untpd.Tree]) = {
      derived.foreach(processDerivedInstance(_))
      addShape()
    }

    def implementedClass(instance: Symbol) =
      instance.info.stripPoly.finalResultType.classSymbol

    def typeclassInstance(sym: Symbol)(tparamRefs: List[Type])(paramRefss: List[List[tpd.Tree]]): tpd.Tree = {
      val tparams = tparamRefs.map(_.typeSymbol.asType)
      val params = if (paramRefss.isEmpty) Nil else paramRefss.head.map(_.symbol.asTerm)
      val typeCls = implementedClass(sym)
      tpd.ref(defn.Predef_undefinedR) // TODO: flesh out
    }

    def syntheticDef(sym: Symbol): tpd.Tree =
      if (sym.isType) tpd.TypeDef(sym.asType)
      else tpd.polyDefDef(sym.asTerm, typeclassInstance(sym))

    def finalize(stat: tpd.TypeDef): tpd.Tree = {
      val templ @ Template(_, _, _, _) = stat.rhs
      val newDefs = synthetics.map(syntheticDef)
      tpd.cpy.TypeDef(stat)(
        rhs = tpd.cpy.Template(templ)(body = templ.body ++ newDefs))
    }
  }
}
