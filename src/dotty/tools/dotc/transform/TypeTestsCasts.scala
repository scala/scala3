package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Constants._
import core.StdNames._
import core.transform.Erasure.isUnboundedGeneric
import typer.ErrorReporting._
import ast.Trees._

/** This transform normalizes type tests and type casts.
 *  Any remaining type tests
 *   - use the object methods $isInstanceOf and $asInstanceOf
 *   - have a reference type as receiver
 *   - can be translated directly to machine instructions
 */
class TypeTestsCasts extends TreeTransform {
  import ast.tpd._

  def box(tree: Tree): Tree = ???

  override def name: String = "typeTestsCasts"

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = ctx.traceIndented(s"transforming ${tree.show}", show = true) {
    tree.fun match {
      case fun @ Select(qual, selector) =>
        val sym = tree.symbol

        def isPrimitive(tp: Type) = tp.classSymbol.isPrimitiveValueClass

        def derivedTree(qual1: Tree, sym: Symbol) =
          cpy.TypeApply(tree, Select(qual1, sym) withPos qual.pos, tree.args)

        def qualCls = qual.tpe.classSymbol

        def transformIsInstanceOf(argType: Type): Tree = {
          if (qual.tpe <:< argType)
            Literal(Constant(true)) withPos tree.pos
          else if (qualCls.isPrimitiveValueClass) {
            val argCls = argType.classSymbol
            if (argCls.isPrimitiveValueClass) Literal(Constant(qualCls == argCls))
            else errorTree(tree, "isInstanceOf cannot test if value types are references")
          }
          else argType.dealias match {
            case _: SingletonType =>
              val cmpOp = if (argType derivesFrom defn.AnyValClass) defn.Any_equals else defn.Object_eq
              Apply(Select(qual, cmpOp), singleton(argType) :: Nil)
            case AndType(tp1, tp2) =>
              evalOnce(fun) { fun =>
                val erased1 = transformIsInstanceOf(tp1)
                val erased2 = transformIsInstanceOf(tp2)
                erased1 match {
                  case Literal(Constant(true)) => erased2
                  case _ =>
                    erased2 match {
                      case Literal(Constant(true)) => erased1
                      case _ => mkAnd(erased1, erased2)
                    }
                }
              }
            case defn.MultiArrayType(elem, ndims) if isUnboundedGeneric(elem) =>
              def isArrayTest(arg: Tree) =
                runtimeCall(nme.isArray, arg :: Literal(Constant(ndims)) :: Nil)
              if (ndims == 1) isArrayTest(qual)
              else evalOnce(qual) { qual1 =>
                mkAnd(derivedTree(qual1, defn.Object_isInstanceOf), isArrayTest(qual1))
              }
            case _ =>
              derivedTree(qual, defn.Object_isInstanceOf)
          }
        }

        def transformAsInstanceOf(argType: Type): Tree = {
          if (qual.tpe <:< argType)
            Typed(qual, tree.args.head)
          else if (qualCls.isPrimitiveValueClass) {
            val argCls = argType.classSymbol
            if (argCls.isPrimitiveValueClass) primitiveConversion(qual, argCls)
            else derivedTree(box(qual), defn.Object_asInstanceOf)
          }
          else
            derivedTree(qual, defn.Object_asInstanceOf)
        }

        if (sym eq defn.Any_isInstanceOf)
          transformIsInstanceOf(tree.args.head.tpe)
        else if (defn.asInstanceOfMethods contains sym)
          transformAsInstanceOf(tree.args.head.tpe)
        else tree

      case _ =>
        tree
    }
  }
}