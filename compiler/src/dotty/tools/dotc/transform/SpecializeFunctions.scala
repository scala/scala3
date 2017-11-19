package dotty.tools.dotc
package transform

import ast.Trees._, ast.tpd, core._
import Contexts.Context, Types._, Decorators._, Symbols._, DenotTransformers._
import SymDenotations._, Scopes._, StdNames._, NameOps._, Names._
import MegaPhase.MiniPhase

import scala.collection.mutable

/** Specializes classes that inherit from `FunctionN` where there exists a
 *  specialized form.
 */
class SpecializeFunctions extends MiniPhase with InfoTransformer {
  import ast.tpd._
  val phaseName = "specializeFunctions"
  override def runsAfter = Set(classOf[ElimByName])

  private val jFunction = "scala.compat.java8.JFunction".toTermName

  /** Transforms the type to include decls for specialized applys  */
  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context) = tp match {
    case tp: ClassInfo if !sym.is(Flags.Package) && (tp.decls ne EmptyScope) && derivesFromFn012(sym) =>
      var newApplys = Map.empty[Name, Symbol]

      var arity = 0
      while (arity < 3) {
        val func = defn.FunctionClass(arity)
        if (tp.derivesFrom(func)) {
          val typeParams = tp.cls.typeRef.baseType(func).argInfos
          val isSpecializable =
            defn.isSpecializableFunction(
              sym.asClass,
              typeParams.init,
              typeParams.last
            )

          if (isSpecializable && tp.decls.lookup(nme.apply).exists) {
            val interface = specInterface(typeParams)
            val specializedMethodName = nme.apply.specializedFunction(typeParams.last, typeParams.init)
            newApplys += (specializedMethodName -> interface)
          }
        }
        arity += 1
      }

      def newDecls =
        newApplys.toList.map { case (name, interface) =>
          ctx.newSymbol(
            sym,
            name,
            Flags.Override | Flags.Method | Flags.Synthetic,
            interface.info.decls.lookup(name).info
          )
        }
        .foldLeft(tp.decls.cloneScope) {
          (scope, sym) => scope.enter(sym); scope
        }

      if (newApplys.isEmpty) tp
      else tp.derivedClassInfo(decls = newDecls)

    case _ => tp
  }

  /** Transforms the `Template` of the classes to contain forwarders from the
   *  generic applys to the specialized ones. Also inserts the specialized applys
   *  in the template body.
   */
  override def transformTemplate(tree: Template)(implicit ctx: Context) = {
    val cls = tree.symbol.enclosingClass.asClass
    if (derivesFromFn012(cls)) {
      val applyBuf = new mutable.ListBuffer[Tree]
      val newBody = tree.body.mapConserve {
        case dt: DefDef if dt.name == nme.apply && dt.vparamss.length == 1 =>
          val typeParams = dt.vparamss.head.map(_.symbol.info)
          val retType = dt.tpe.widen.finalResultType

          val specName = specializedName(nme.apply, typeParams :+ retType)
          val specializedApply = cls.info.decls.lookup(specName)
          if (specializedApply.exists) {
            val apply = specializedApply.asTerm
            val specializedDecl =
              polyDefDef(apply, trefs => vrefss => {
                dt.rhs
                  .changeOwner(dt.symbol, apply)
                  .subst(dt.vparamss.flatten.map(_.symbol), vrefss.flatten.map(_.symbol))
              })
            applyBuf += specializedDecl

            // create a forwarding to the specialized apply
            cpy.DefDef(dt)(rhs = {
              tpd
                .ref(apply)
                .appliedToArgs(dt.vparamss.head.map(vparam => ref(vparam.symbol)))
            })
          } else dt

        case x => x
      }

      cpy.Template(tree)(
        body = applyBuf.toList ::: newBody
      )
    } else tree
  }

  /** Dispatch to specialized `apply`s in user code when available */
  override def transformApply(tree: Apply)(implicit ctx: Context) =
    tree match {
      case Apply(fun, args)
        if fun.symbol.name == nme.apply &&
        fun.symbol.owner.derivesFrom(defn.FunctionClass(args.length))
      =>
        val params = (fun.tpe.widen.firstParamTypes :+ tree.tpe).map(_.widenSingleton.dealias)
        val isSpecializable =
          defn.isSpecializableFunction(
            fun.symbol.owner.asClass,
            params.init,
            params.last)

        if (isSpecializable && !params.exists(_.isInstanceOf[ExprType])) {
          val specializedApply = specializedName(nme.apply, params)
          val newSel = fun match {
            case Select(qual, _) =>
              qual.select(specializedApply)
            case _ =>
              (fun.tpe: @unchecked) match {
                case TermRef(prefix: ThisType, name) =>
                  tpd.This(prefix.cls).select(specializedApply)
                case TermRef(prefix: NamedType, name) =>
                  tpd.ref(prefix).select(specializedApply)
              }
          }

          newSel.appliedToArgs(args)
        }
        else tree

      case _ => tree
    }

  private def specializedName(name: Name, args: List[Type])(implicit ctx: Context) =
    name.specializedFunction(args.last, args.init)

  private def functionName(typeParams: List[Type])(implicit ctx: Context) =
    jFunction ++ (typeParams.length - 1).toString

  private def specInterface(typeParams: List[Type])(implicit ctx: Context) =
    ctx.getClassIfDefined(functionName(typeParams).specializedFunction(typeParams.last, typeParams.init))

  private def derivesFromFn012(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.derivesFrom(defn.FunctionClass(0)) ||
      sym.derivesFrom(defn.FunctionClass(1)) ||
      sym.derivesFrom(defn.FunctionClass(2))
}
