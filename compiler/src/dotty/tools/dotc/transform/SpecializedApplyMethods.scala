package dotty.tools.dotc
package transform

import TreeTransforms.{ MiniPhaseTransform, TransformerInfo }
import ast.Trees._, ast.tpd, core._
import Contexts.Context, Types._, Decorators._, Symbols._, DenotTransformers._
import SymDenotations._, Scopes._, StdNames._, NameOps._, Names._

import scala.reflect.internal.util.Collections

/** This phase synthesizes specialized methods for FunctionN, this is done
 *  since there are no scala signatures in the bytecode for the specialized
 *  methods.
 *
 *  We know which specializations exist for the different arities, therefore we
 *  can hardcode them. This should, however be removed once we're using a
 *  different standard library.
 */
class SpecializedApplyMethods extends MiniPhaseTransform with InfoTransformer {
  import ast.tpd._

  val phaseName = "specializedApplyMethods"

  private[this] var func0Applys: List[Symbol] = _
  private[this] var func1Applys: List[Symbol] = _
  private[this] var func2Applys: List[Symbol] = _
  private[this] var func0: Symbol = _
  private[this] var func1: Symbol = _
  private[this] var func2: Symbol = _

  private def init()(implicit ctx: Context): Unit = if (func0Applys eq null) {
    def specApply(sym: Symbol, ret: Type, args: List[Type])(implicit ctx: Context) = {
      val all = args :+ ret
      val name = nme.apply.specializedFor(all, all.map(_.typeSymbol.name), Nil, Nil)
      ctx.newSymbol(sym, name, Flags.Method, MethodType(args, ret))
    }

    func0 = defn.FunctionClass(0)
    func0Applys = List(
      specApply(func0, defn.UnitType,    Nil),
      specApply(func0, defn.ByteType,    Nil),
      specApply(func0, defn.ShortType,   Nil),
      specApply(func0, defn.IntType,     Nil),
      specApply(func0, defn.LongType,    Nil),
      specApply(func0, defn.CharType,    Nil),
      specApply(func0, defn.FloatType,   Nil),
      specApply(func0, defn.DoubleType,  Nil),
      specApply(func0, defn.BooleanType, Nil)
    )
    func1 = defn.FunctionClass(1)
    func1Applys = List(
      specApply(func1, defn.UnitType,    List(defn.IntType)),
      specApply(func1, defn.IntType,     List(defn.IntType)),
      specApply(func1, defn.FloatType,   List(defn.IntType)),
      specApply(func1, defn.LongType,    List(defn.IntType)),
      specApply(func1, defn.DoubleType,  List(defn.IntType)),
      specApply(func1, defn.UnitType,    List(defn.LongType)),
      specApply(func1, defn.BooleanType, List(defn.LongType)),
      specApply(func1, defn.IntType,     List(defn.LongType)),
      specApply(func1, defn.FloatType,   List(defn.LongType)),
      specApply(func1, defn.LongType,    List(defn.LongType)),
      specApply(func1, defn.DoubleType,  List(defn.LongType)),
      specApply(func1, defn.UnitType,    List(defn.FloatType)),
      specApply(func1, defn.BooleanType, List(defn.FloatType)),
      specApply(func1, defn.IntType,     List(defn.FloatType)),
      specApply(func1, defn.FloatType,   List(defn.FloatType)),
      specApply(func1, defn.LongType,    List(defn.FloatType)),
      specApply(func1, defn.DoubleType,  List(defn.FloatType)),
      specApply(func1, defn.UnitType,    List(defn.DoubleType)),
      specApply(func1, defn.BooleanType, List(defn.DoubleType)),
      specApply(func1, defn.IntType,     List(defn.DoubleType)),
      specApply(func1, defn.FloatType,   List(defn.DoubleType)),
      specApply(func1, defn.LongType,    List(defn.DoubleType)),
      specApply(func1, defn.DoubleType,  List(defn.DoubleType))
    )
    func2 = defn.FunctionClass(2)
    func2Applys = List(
      specApply(func2, defn.UnitType,    List(defn.IntType, defn.IntType)),
      specApply(func2, defn.BooleanType, List(defn.IntType, defn.IntType)),
      specApply(func2, defn.IntType,     List(defn.IntType, defn.IntType)),
      specApply(func2, defn.FloatType,   List(defn.IntType, defn.IntType)),
      specApply(func2, defn.LongType,    List(defn.IntType, defn.IntType)),
      specApply(func2, defn.DoubleType,  List(defn.IntType, defn.IntType)),
      specApply(func2, defn.UnitType,    List(defn.IntType, defn.LongType)),
      specApply(func2, defn.BooleanType, List(defn.IntType, defn.LongType)),
      specApply(func2, defn.IntType,     List(defn.IntType, defn.LongType)),
      specApply(func2, defn.FloatType,   List(defn.IntType, defn.LongType)),
      specApply(func2, defn.LongType,    List(defn.IntType, defn.LongType)),
      specApply(func2, defn.DoubleType,  List(defn.IntType, defn.LongType)),
      specApply(func2, defn.UnitType,    List(defn.IntType, defn.DoubleType)),
      specApply(func2, defn.BooleanType, List(defn.IntType, defn.DoubleType)),
      specApply(func2, defn.IntType,     List(defn.IntType, defn.DoubleType)),
      specApply(func2, defn.FloatType,   List(defn.IntType, defn.DoubleType)),
      specApply(func2, defn.LongType,    List(defn.IntType, defn.DoubleType)),
      specApply(func2, defn.DoubleType,  List(defn.IntType, defn.DoubleType)),
      specApply(func2, defn.UnitType,    List(defn.LongType, defn.IntType)),
      specApply(func2, defn.BooleanType, List(defn.LongType, defn.IntType)),
      specApply(func2, defn.IntType,     List(defn.LongType, defn.IntType)),
      specApply(func2, defn.FloatType,   List(defn.LongType, defn.IntType)),
      specApply(func2, defn.LongType,    List(defn.LongType, defn.IntType)),
      specApply(func2, defn.DoubleType,  List(defn.LongType, defn.IntType)),
      specApply(func2, defn.UnitType,    List(defn.LongType, defn.LongType)),
      specApply(func2, defn.BooleanType, List(defn.LongType, defn.LongType)),
      specApply(func2, defn.IntType,     List(defn.LongType, defn.LongType)),
      specApply(func2, defn.FloatType,   List(defn.LongType, defn.LongType)),
      specApply(func2, defn.LongType,    List(defn.LongType, defn.LongType)),
      specApply(func2, defn.DoubleType,  List(defn.LongType, defn.LongType)),
      specApply(func2, defn.UnitType,    List(defn.LongType, defn.DoubleType)),
      specApply(func2, defn.BooleanType, List(defn.LongType, defn.DoubleType)),
      specApply(func2, defn.IntType,     List(defn.LongType, defn.DoubleType)),
      specApply(func2, defn.FloatType,   List(defn.LongType, defn.DoubleType)),
      specApply(func2, defn.LongType,    List(defn.LongType, defn.DoubleType)),
      specApply(func2, defn.DoubleType,  List(defn.LongType, defn.DoubleType)),
      specApply(func2, defn.UnitType,    List(defn.DoubleType, defn.IntType)),
      specApply(func2, defn.BooleanType, List(defn.DoubleType, defn.IntType)),
      specApply(func2, defn.IntType,     List(defn.DoubleType, defn.IntType)),
      specApply(func2, defn.FloatType,   List(defn.DoubleType, defn.IntType)),
      specApply(func2, defn.LongType,    List(defn.DoubleType, defn.IntType)),
      specApply(func2, defn.DoubleType,  List(defn.DoubleType, defn.IntType)),
      specApply(func2, defn.UnitType,    List(defn.DoubleType, defn.LongType)),
      specApply(func2, defn.BooleanType, List(defn.DoubleType, defn.LongType)),
      specApply(func2, defn.IntType,     List(defn.DoubleType, defn.LongType)),
      specApply(func2, defn.FloatType,   List(defn.DoubleType, defn.LongType)),
      specApply(func2, defn.LongType,    List(defn.DoubleType, defn.LongType)),
      specApply(func2, defn.DoubleType,  List(defn.DoubleType, defn.LongType)),
      specApply(func2, defn.UnitType,    List(defn.DoubleType, defn.DoubleType)),
      specApply(func2, defn.BooleanType, List(defn.DoubleType, defn.DoubleType)),
      specApply(func2, defn.IntType,     List(defn.DoubleType, defn.DoubleType)),
      specApply(func2, defn.FloatType,   List(defn.DoubleType, defn.DoubleType)),
      specApply(func2, defn.LongType,    List(defn.DoubleType, defn.DoubleType)),
      specApply(func2, defn.DoubleType,  List(defn.DoubleType, defn.DoubleType))
    )
  }

  /** Add symbols for specialized methods to FunctionN */
  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context) = tp match {
    case tp: ClassInfo if defn.isFunctionClass(sym) => {
      init()
      val newDecls = sym.name.functionArity match {
        case 0 => func0Applys.foldLeft(tp.decls.cloneScope) {
          (decls, sym) => decls.enter(sym); decls
        }
        case 1 => func1Applys.foldLeft(tp.decls.cloneScope) {
          (decls, sym) => decls.enter(sym); decls
        }
        case 2 => func2Applys.foldLeft(tp.decls.cloneScope) {
          (decls, sym) => decls.enter(sym); decls
        }
        case _ => tp.decls
      }

      tp.derivedClassInfo(decls = newDecls)
    }
    case _ => tp
  }

  /** Create bridge methods for FunctionN with specialized applys */
  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo) = {
    val owner = tree.symbol.owner
    val additionalSymbols =
      if (owner eq func0) func0Applys
      else if (owner eq func1) func1Applys
      else if (owner eq func2) func2Applys
      else Nil

    if (additionalSymbols eq Nil) tree
    else {
      val newBody: List[Tree] = tree.body ++ additionalSymbols.map { applySym =>
        polyDefDef(applySym.asTerm, tparams => vparamss => {
          val prefix = This(owner.asClass).select(nme.apply).appliedToTypes(vparamss.head.map(_.tpe))
          val argTypess = prefix.tpe.widen.paramTypess

          val argss = Collections.map2(vparamss, argTypess) { (vparams, argTypes) =>
            Collections.map2(vparams, argTypes) { (vparam, argType) => vparam.ensureConforms(argType) }
          }
          prefix.appliedToArgss(argss).ensureConforms(applySym.info.finalResultType)
        })
      }

      cpy.Template(tree)(body = newBody)
    }
  }
}
