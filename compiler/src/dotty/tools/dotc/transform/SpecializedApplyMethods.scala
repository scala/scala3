package dotty.tools.dotc
package transform

import TreeTransforms.{ MiniPhaseTransform, TransformerInfo }
import ast.Trees._, ast.tpd, core._
import Contexts.Context, Types._, Decorators._, Symbols._, DenotTransformers._
import SymDenotations._, Scopes._, StdNames._, NameOps._, Names._

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

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context) = tp match {
    case tp: ClassInfo if defn.isFunctionClass(sym) => {
      def specApply(ret: Type, args: List[Type])(implicit ctx: Context) = {
        val all = args :+ ret
        val name = nme.apply.specializedFor(all, all.map(_.typeSymbol.name), Nil, Nil)
        ctx.newSymbol(sym, name, Flags.Method, MethodType(args, ret))
      }

      val newDecls = sym.name.functionArity match {
        case 0 =>
          List(
            specApply(defn.UnitType,    Nil),
            specApply(defn.ByteType,    Nil),
            specApply(defn.ShortType,   Nil),
            specApply(defn.IntType,     Nil),
            specApply(defn.LongType,    Nil),
            specApply(defn.CharType,    Nil),
            specApply(defn.FloatType,   Nil),
            specApply(defn.DoubleType,  Nil),
            specApply(defn.BooleanType, Nil)
          )
          .foldLeft(tp.decls.cloneScope){ (decls, sym) => decls.enter(sym); decls }

        case 1 =>
          List(
            specApply(defn.UnitType,    List(defn.IntType)),
            specApply(defn.IntType,     List(defn.IntType)),
            specApply(defn.FloatType,   List(defn.IntType)),
            specApply(defn.LongType,    List(defn.IntType)),
            specApply(defn.DoubleType,  List(defn.IntType)),
            specApply(defn.UnitType,    List(defn.LongType)),
            specApply(defn.BooleanType, List(defn.LongType)),
            specApply(defn.IntType,     List(defn.LongType)),
            specApply(defn.FloatType,   List(defn.LongType)),
            specApply(defn.LongType,    List(defn.LongType)),
            specApply(defn.DoubleType,  List(defn.LongType)),
            specApply(defn.UnitType,    List(defn.FloatType)),
            specApply(defn.BooleanType, List(defn.FloatType)),
            specApply(defn.IntType,     List(defn.FloatType)),
            specApply(defn.FloatType,   List(defn.FloatType)),
            specApply(defn.LongType,    List(defn.FloatType)),
            specApply(defn.DoubleType,  List(defn.FloatType)),
            specApply(defn.UnitType,    List(defn.DoubleType)),
            specApply(defn.BooleanType, List(defn.DoubleType)),
            specApply(defn.IntType,     List(defn.DoubleType)),
            specApply(defn.FloatType,   List(defn.DoubleType)),
            specApply(defn.LongType,    List(defn.DoubleType)),
            specApply(defn.DoubleType,  List(defn.DoubleType))
          )
          .foldLeft(tp.decls.cloneScope){ (decls, sym) => decls.enter(sym); decls }

        case 2 =>
          List(
            specApply(defn.UnitType,    List(defn.IntType, defn.IntType)),
            specApply(defn.BooleanType, List(defn.IntType, defn.IntType)),
            specApply(defn.IntType,     List(defn.IntType, defn.IntType)),
            specApply(defn.FloatType,   List(defn.IntType, defn.IntType)),
            specApply(defn.LongType,    List(defn.IntType, defn.IntType)),
            specApply(defn.DoubleType,  List(defn.IntType, defn.IntType)),
            specApply(defn.UnitType,    List(defn.IntType, defn.LongType)),
            specApply(defn.BooleanType, List(defn.IntType, defn.LongType)),
            specApply(defn.IntType,     List(defn.IntType, defn.LongType)),
            specApply(defn.FloatType,   List(defn.IntType, defn.LongType)),
            specApply(defn.LongType,    List(defn.IntType, defn.LongType)),
            specApply(defn.DoubleType,  List(defn.IntType, defn.LongType)),
            specApply(defn.UnitType,    List(defn.IntType, defn.DoubleType)),
            specApply(defn.BooleanType, List(defn.IntType, defn.DoubleType)),
            specApply(defn.IntType,     List(defn.IntType, defn.DoubleType)),
            specApply(defn.FloatType,   List(defn.IntType, defn.DoubleType)),
            specApply(defn.LongType,    List(defn.IntType, defn.DoubleType)),
            specApply(defn.DoubleType,  List(defn.IntType, defn.DoubleType)),
            specApply(defn.UnitType,    List(defn.LongType, defn.IntType)),
            specApply(defn.BooleanType, List(defn.LongType, defn.IntType)),
            specApply(defn.IntType,     List(defn.LongType, defn.IntType)),
            specApply(defn.FloatType,   List(defn.LongType, defn.IntType)),
            specApply(defn.LongType,    List(defn.LongType, defn.IntType)),
            specApply(defn.DoubleType,  List(defn.LongType, defn.IntType)),
            specApply(defn.UnitType,    List(defn.LongType, defn.LongType)),
            specApply(defn.BooleanType, List(defn.LongType, defn.LongType)),
            specApply(defn.IntType,     List(defn.LongType, defn.LongType)),
            specApply(defn.FloatType,   List(defn.LongType, defn.LongType)),
            specApply(defn.LongType,    List(defn.LongType, defn.LongType)),
            specApply(defn.DoubleType,  List(defn.LongType, defn.LongType)),
            specApply(defn.UnitType,    List(defn.LongType, defn.DoubleType)),
            specApply(defn.BooleanType, List(defn.LongType, defn.DoubleType)),
            specApply(defn.IntType,     List(defn.LongType, defn.DoubleType)),
            specApply(defn.FloatType,   List(defn.LongType, defn.DoubleType)),
            specApply(defn.LongType,    List(defn.LongType, defn.DoubleType)),
            specApply(defn.DoubleType,  List(defn.LongType, defn.DoubleType)),
            specApply(defn.UnitType,    List(defn.DoubleType, defn.IntType)),
            specApply(defn.BooleanType, List(defn.DoubleType, defn.IntType)),
            specApply(defn.IntType,     List(defn.DoubleType, defn.IntType)),
            specApply(defn.FloatType,   List(defn.DoubleType, defn.IntType)),
            specApply(defn.LongType,    List(defn.DoubleType, defn.IntType)),
            specApply(defn.DoubleType,  List(defn.DoubleType, defn.IntType)),
            specApply(defn.UnitType,    List(defn.DoubleType, defn.LongType)),
            specApply(defn.BooleanType, List(defn.DoubleType, defn.LongType)),
            specApply(defn.IntType,     List(defn.DoubleType, defn.LongType)),
            specApply(defn.FloatType,   List(defn.DoubleType, defn.LongType)),
            specApply(defn.LongType,    List(defn.DoubleType, defn.LongType)),
            specApply(defn.DoubleType,  List(defn.DoubleType, defn.LongType)),
            specApply(defn.UnitType,    List(defn.DoubleType, defn.DoubleType)),
            specApply(defn.BooleanType, List(defn.DoubleType, defn.DoubleType)),
            specApply(defn.IntType,     List(defn.DoubleType, defn.DoubleType)),
            specApply(defn.FloatType,   List(defn.DoubleType, defn.DoubleType)),
            specApply(defn.LongType,    List(defn.DoubleType, defn.DoubleType)),
            specApply(defn.DoubleType,  List(defn.DoubleType, defn.DoubleType))
          )
          .foldLeft(tp.decls.cloneScope){ (decls, sym) => decls.enter(sym); decls }

        case _ =>
          tp.decls
      }

      tp.derivedClassInfo(decls = newDecls)
    }
    case _ => tp
  }
}
