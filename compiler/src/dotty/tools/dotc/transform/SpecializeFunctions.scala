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

  /** Transforms the type to include decls for specialized applys and replace
   *  the class parents with specialized versions.
   */
  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context) = tp match {
    case tp: ClassInfo if !sym.is(Flags.Package) => {
      var newApplys: List[Symbol] = Nil

      val newParents = tp.parents.mapConserve { parent =>
        if (defn.isPlainFunctionClass(parent.symbol)) {
          val typeParams = tp.typeRef.baseArgTypes(parent.classSymbol)
          val interface = specInterface(typeParams)

          if (interface.exists) {
            val specializedApply: Symbol = {
              val specializedMethodName = specializedName(nme.apply, typeParams)
              ctx.newSymbol(
                sym,
                specializedMethodName,
                Flags.Override | Flags.Method,
                interface.info.decls.lookup(specializedMethodName).info
              )
            }

            newApplys = specializedApply :: newApplys
            interface.typeRef
          }
          else parent
        }
        else parent
      }

      def newDecls = newApplys.foldLeft(tp.decls.cloneScope) {
        (scope, sym) => scope.enter(sym); scope
      }

      if (newApplys eq Nil) tp
      else tp.derivedClassInfo(classParents = newParents, decls = newDecls)
    }

    case _ => tp
  }

  /** Transforms the `Template` of the classes to contain forwarders from the
   *  generic applys to the specialized ones. Also replaces parents of the
   *  class on the tree level and inserts the specialized applys in the
   *  template body.
   */
  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo) = {
    val buf = new mutable.ListBuffer[Tree]
    val newBody = tree.body.mapConserve {
      case dt: DefDef if dt.name == nme.apply && dt.vparamss.length == 1 => {
        val specializedApply = ctx.owner.info.decls.lookup {
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

          buf += specializedDecl

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

    val newParents = tree.parents.mapConserve { parent =>
        if (defn.isPlainFunctionClass(parent.symbol)) {
          val typeParams = tree.tpe.baseArgTypes(parent.symbol)
          val interface = specInterface(typeParams)

          if (interface.exists) TypeTree(interface.info)
          else parent
        }
        else parent
    }

    cpy.Template(tree)(parents = newParents, body = buf.toList ++ newBody)
  }

  /** Dispatch to specialized `apply`s in user code when available */
  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo) =
    tree match {
      case Apply(select @ Select(id, nme.apply), args) => {
        val params = args.map(_.tpe) :+ tree.tpe
        val specializedApply = specializedName(nme.apply, params)

        if (tree.fun.symbol.owner.info.member(specializedApply).exists)
          tpd.Apply(tpd.Select(id, specializedApply), args)
        else tree
      }
      case _ => tree
    }

  @inline private def specializedName(name: Name, args: List[Type])(implicit ctx: Context) =
    name.specializedFor(args, args.map(_.typeSymbol.name), Nil, Nil)

  @inline private def specInterface(typeParams: List[Type])(implicit ctx: Context) = {
    val specName =
      ("JFunction".toTermName ++ (typeParams.length - 1))
      .specializedFor(typeParams, typeParams.map(_.typeSymbol.name), Nil, Nil)

    ctx.getClassIfDefined("scala.compat.java8.".toTermName ++ specName)
  }
}
