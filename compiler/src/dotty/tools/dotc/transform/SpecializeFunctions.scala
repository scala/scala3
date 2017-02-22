package dotty.tools.dotc
package transform

import TreeTransforms.{ MiniPhaseTransform, TransformerInfo }
import ast.Trees._, ast.tpd, core._
import Contexts.Context, Types._, Decorators._, Symbols._, DenotTransformers._
import SymDenotations._, Scopes._, StdNames._, NameOps._, Names._

import scala.collection.mutable

/** Specializes classes that inherit from `FunctionN` where there exists a
 *  specialized form.
 */
class SpecializeFunctions extends MiniPhaseTransform with InfoTransformer {
  import ast.tpd._
  val phaseName = "specializeFunctions"

  private[this] var _blacklistedSymbols: List[Symbol] = _

  private def blacklistedSymbols(implicit ctx: Context): List[Symbol] = {
    if (_blacklistedSymbols eq null) _blacklistedSymbols = List(
      ctx.getClassIfDefined("scala.math.Ordering").asClass.membersNamed("Ops".toTypeName).first.symbol
    )

    _blacklistedSymbols
  }

  /** Transforms the type to include decls for specialized applys and replace
   *  the class parents with specialized versions.
   */
  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context) = tp match {
    case tp: ClassInfo
      if !sym.is(Flags.Package) &&
         !blacklistedSymbols.contains(sym) &&
         (tp.decls ne EmptyScope)
    => {
      var newApplys: List[Symbol] = Nil

      val newParents = tp.parents.mapConserve { parent =>
        List(0, 1, 2, 3).flatMap { arity =>
          val func = defn.FunctionClass(arity)
          if (!parent.isRef(func)) Nil
          else {
            val typeParams = tp.typeRef.baseArgInfos(func)
            val interface = specInterface(typeParams)

            if (interface.exists) {
              val specializedApply = {
                val specializedMethodName = specializedName(nme.apply, typeParams)
                ctx.newSymbol(
                  sym,
                  specializedMethodName,
                  Flags.Override | Flags.Method,
                  interface.info.decls.lookup(specializedMethodName).info
                )
              }
              newApplys = specializedApply :: newApplys
              List(interface.typeRef)
            }
            else Nil
          }
        }
        .headOption
        .getOrElse(parent)
      }

      def newDecls =
        if (newApplys.isEmpty) tp.decls
        else newApplys.foldLeft(tp.decls.cloneScope) {
          (scope, sym) => scope.enter(sym); scope
        }

      tp.derivedClassInfo(
        classParents = newParents,
        decls = newDecls
      )
    }

    case _ => tp
  }

  /** Transforms the `Template` of the classes to contain forwarders from the
   *  generic applys to the specialized ones. Also replaces parents of the
   *  class on the tree level and inserts the specialized applys in the
   *  template body.
   */
  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo) = {
    val applyBuf = new mutable.ListBuffer[Tree]
    val newBody = tree.body.mapConserve {
      case dt: DefDef if dt.name == nme.apply && dt.vparamss.length == 1 => {
        val specializedApply = tree.symbol.enclosingClass.info.decls.lookup {
          specializedName(
            nme.apply,
            dt.vparamss.head.map(_.symbol.info) :+ dt.tpe.widen.finalResultType
          )
        }

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
      }
      case x => x
    }

    val missing: List[TypeTree] = List(0, 1, 2, 3).flatMap { arity =>
      val func = defn.FunctionClass(arity)
      val tr = tree.symbol.enclosingClass.typeRef

      if (!tr.parents.exists(_.isRef(func))) Nil
      else {
        val typeParams = tr.baseArgInfos(func)
        val interface = specInterface(typeParams)

        if (interface.exists) List(interface.info)
        else Nil
      }
    }.map(TypeTree)

    cpy.Template(tree)(
      parents = tree.parents ++ missing,
      body = applyBuf.toList ++ newBody
    )
  }

  /** Dispatch to specialized `apply`s in user code when available */
  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo) =
    tree match {
      case app @ Apply(fun, args)
        if fun.symbol.name == nme.apply &&
        fun.symbol.owner.derivesFrom(defn.FunctionClass(args.length))
      => {
        val params = (fun.tpe.widen.firstParamTypes :+ tree.tpe).map(_.widenSingleton.dealias)
        val specializedApply = specializedName(nme.apply, params)

        if (!params.exists(_.isInstanceOf[ExprType]) && defn.FunctionClass(args.length).info.member(specializedApply).exists) {
          val newSel = fun match {
            case Select(qual, _) =>
              qual.select(specializedApply)
            case _ => {
              (fun.tpe: @unchecked) match {
                case TermRef(prefix: ThisType, name) =>
                  tpd.This(prefix.cls).select(specializedApply)
                case TermRef(prefix: NamedType, name) =>
                  tpd.ref(prefix).select(specializedApply)
              }
            }
          }

          newSel.appliedToArgs(args)
        }
        else tree
      }
      case _ => tree
    }

  @inline private def specializedName(name: Name, args: List[Type])(implicit ctx: Context) =
    name.specializedFor(args, args.map(_.typeSymbol.name), Nil, Nil)

  @inline private def specInterface(typeParams: List[Type])(implicit ctx: Context) = {
    val specName =
      ("JFunction" + (typeParams.length - 1)).toTermName
      .specializedFor(typeParams, typeParams.map(_.typeSymbol.name), Nil, Nil)

    ctx.getClassIfDefined("scala.compat.java8.".toTermName ++ specName)
  }
}
