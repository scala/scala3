package dotty.tools.dotc
package transform

import TreeTransforms._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Constants._
import core.StdNames._
import core.TypeErasure.isUnboundedGeneric
import typer.ErrorReporting._
import ast.Trees._
import Erasure.Boxing._
import core.TypeErasure._
import ValueClasses._

/** This transform normalizes type tests and type casts,
 *  also replacing type tests with singleton argument type with reference equality check
 *  Any remaining type tests
 *   - use the object methods $isInstanceOf and $asInstanceOf
 *   - have a reference type as receiver
 *   - can be translated directly to machine instructions
 *
 *
 * Unfortunately this phase ended up being not Y-checkable unless types are erased. A cast to an ConstantType(3) or x.type
 * cannot be rewritten before erasure.
 */
trait TypeTestsCasts {
  import ast.tpd._

  // override def phaseName: String = "typeTestsCasts"

  def interceptTypeApply(tree: TypeApply)(implicit ctx: Context): Tree = ctx.traceIndented(s"transforming ${tree.show}", show = true) {
    tree.fun match {
      case fun @ Select(qual, selector) =>
        val sym = tree.symbol

        def isPrimitive(tp: Type) = tp.classSymbol.isPrimitiveValueClass

        def derivedTree(qual1: Tree, sym: Symbol, tp: Type) =
          cpy.TypeApply(tree)(qual1.select(sym).withPos(qual.pos), List(TypeTree(tp)))

        def qualCls = qual.tpe.widen.classSymbol

        def transformIsInstanceOf(expr:Tree, argType: Type): Tree = {
          def argCls = argType.classSymbol
          if ((expr.tpe <:< argType) && isPureExpr(expr))
            Literal(Constant(true)) withPos tree.pos
          else if (argCls.isPrimitiveValueClass)
            if (qualCls.isPrimitiveValueClass) Literal(Constant(qualCls == argCls)) withPos tree.pos
            else transformIsInstanceOf(expr, defn.boxedType(argCls.typeRef))
          else argType.dealias match {
            case _: SingletonType =>
              val cmpOp = if (argType derivesFrom defn.AnyValClass) defn.Any_equals else defn.Object_eq
              expr.select(cmpOp).appliedTo(singleton(argType))
            case AndType(tp1, tp2) =>
              evalOnce(expr) { fun =>
                val erased1 = transformIsInstanceOf(fun, tp1)
                val erased2 = transformIsInstanceOf(fun, tp2)
                erased1 match {
                  case Literal(Constant(true)) => erased2
                  case _ =>
                    erased2 match {
                      case Literal(Constant(true)) => erased1
                      case _ => erased1 and erased2
                    }
                }
              }
            case defn.MultiArrayOf(elem, ndims) if isUnboundedGeneric(elem) =>
              def isArrayTest(arg: Tree) =
                ref(defn.runtimeMethodRef(nme.isArray)).appliedTo(arg, Literal(Constant(ndims)))
              if (ndims == 1) isArrayTest(qual)
              else evalOnce(qual) { qual1 =>
                derivedTree(qual1, defn.Any_isInstanceOf, qual1.tpe) and isArrayTest(qual1)
              }
            case _ =>
              derivedTree(expr, defn.Any_isInstanceOf, argType)
          }
        }

        def transformAsInstanceOf(argType: Type): Tree = {
          def argCls = argType.widen.classSymbol
          if (qual.tpe <:< argType)
            Typed(qual, tree.args.head)
          else if (qualCls.isPrimitiveValueClass) {
            if (argCls.isPrimitiveValueClass) primitiveConversion(qual, argCls)
            else derivedTree(box(qual), defn.Any_asInstanceOf, argType)
          }
          else if (argCls.isPrimitiveValueClass)
            unbox(qual.ensureConforms(defn.ObjectType), argType)
          else if (isDerivedValueClass(argCls)) {
            qual // adaptToType in Erasure will do the necessary type adaptation
          } else
            derivedTree(qual, defn.Any_asInstanceOf, argType)
        }
        def erasedArg = erasure(tree.args.head.tpe)
        if (sym eq defn.Any_isInstanceOf)
          transformIsInstanceOf(qual, erasedArg)
        else if (sym eq defn.Any_asInstanceOf)
          transformAsInstanceOf(erasedArg)
        else tree

      case _ =>
        tree
    }
  }
}
