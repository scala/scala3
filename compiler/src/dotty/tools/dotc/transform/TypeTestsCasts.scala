package dotty.tools.dotc
package transform

import core._
import Contexts._, Symbols._, Types._, Constants._, StdNames._, Decorators._
import ast.Trees._
import Erasure.Boxing._
import TypeErasure._
import ValueClasses._
import core.Flags._
import util.Positions._


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

  def interceptTypeApply(tree: TypeApply)(implicit ctx: Context): Tree = ctx.traceIndented(s"transforming ${tree.show}", show = true) {
    tree.fun match {
      case fun @ Select(qual, selector) =>
        val sym = tree.symbol

        def isPrimitive(tp: Type) = tp.classSymbol.isPrimitiveValueClass

        def derivedTree(qual1: Tree, sym: Symbol, tp: Type) =
          cpy.TypeApply(tree)(qual1.select(sym).withPos(qual.pos), List(TypeTree(tp)))

        def foundCls = qual.tpe.widen.classSymbol
        // println(i"ta $tree, found = $foundCls")

        def inMatch =
          fun.symbol == defn.Any_typeTest ||  // new scheme
          qual.symbol.is(Case)                // old scheme

        def transformIsInstanceOf(expr:Tree, testType: Type, flagUnrelated: Boolean): Tree = {
          def testCls = testType.classSymbol

          def unreachable(why: => String) =
            if (flagUnrelated)
              if (inMatch) ctx.error(em"this case is unreachable since $why", expr.pos)
              else ctx.warning(em"this will always yield false since $why", expr.pos)

          def checkSensical: Boolean = {
            val foundCls = erasure(qual.tpe.widen).typeSymbol
            val testCls = testType.typeSymbol
            !foundCls.isClass ||
            !testCls.isClass ||
            testCls.isPrimitiveValueClass ||
            ValueClasses.isDerivedValueClass(foundCls) ||
            ValueClasses.isDerivedValueClass(testCls) ||
            foundCls == defn.ObjectClass ||
            foundCls.derivesFrom(testCls) || {
              if (foundCls.is(Final)) {
                unreachable(i"$foundCls is not a subclass of $testCls")
                false
              }
              else if (!testCls.derivesFrom(foundCls) &&
                       (testCls.is(Final) || !testCls.is(Trait) && !foundCls.is(Trait))) {
                unreachable(i"$foundCls and $testCls are unrelated")
                false
              }
              else true
            }
          }
          println(i"test ii $tree ${expr.tpe} $testType")

          if (expr.tpe <:< testType)
            if (expr.tpe.isNotNull) {
              ctx.warning(
                em"this will always yield true, since `$foundCls` is a subclass of `$testCls`",
                expr.pos)
              constant(expr, Literal(Constant(true)))
            }
            else expr.testNotNull
          else if (!checkSensical)
            constant(expr, Literal(Constant(false)))
          else if (testCls.isPrimitiveValueClass)
            if (foundCls.isPrimitiveValueClass)
              constant(expr, Literal(Constant(foundCls == testCls)))
            else
              transformIsInstanceOf(expr, defn.boxedType(testCls.typeRef), flagUnrelated)
          else testType.dealias match {
            case _: SingletonType =>
              val cmpOp =
                if (testType derivesFrom defn.AnyValClass) defn.Any_equals
                else defn.Object_eq
              expr.select(cmpOp).appliedTo(singleton(testType))
            case _ =>
              derivedTree(expr, defn.Any_isInstanceOf, testType)
          }
        }

        def transformAsInstanceOf(testType: Type): Tree = {
          def testCls = testType.widen.classSymbol
          if (qual.tpe <:< testType)
            Typed(qual, tree.args.head)
          else if (foundCls.isPrimitiveValueClass) {
            if (testCls.isPrimitiveValueClass) primitiveConversion(qual, testCls)
            else derivedTree(box(qual), defn.Any_asInstanceOf, testType)
          }
          else if (testCls.isPrimitiveValueClass)
            unbox(qual.ensureConforms(defn.ObjectType), testType)
          else if (isDerivedValueClass(testCls)) {
            qual // adaptToType in Erasure will do the necessary type adaptation
          }
          else
            derivedTree(qual, defn.Any_asInstanceOf, testType)
        }

        /** Transform isInstanceOf OrType
         *
         *    expr.isInstanceOf[A | B]  ~~>  expr.isInstanceOf[A] | expr.isInstanceOf[B]
         *    expr.isInstanceOf[A & B]  ~~>  expr.isInstanceOf[A] & expr.isInstanceOf[B]
         *
         *  The transform happens before erasure of `testType`, thus cannot be merged
         *  with `transformIsInstanceOf`, which depends on erased type of `testType`.
         */
        def transformTypeTest(qual: Tree, testType: Type, flagUnrelated: Boolean): Tree = testType.dealias match {
          case OrType(tp1, tp2) =>
            evalOnce(qual) { fun =>
              transformTypeTest(fun, tp1, flagUnrelated = false)
                .select(defn.Boolean_||)
                .appliedTo(transformTypeTest(fun, tp2, flagUnrelated = false))
            }
          case AndType(tp1, tp2) =>
            evalOnce(qual) { fun =>
              transformTypeTest(fun, tp1, flagUnrelated)
                .select(defn.Boolean_&&)
                .appliedTo(transformTypeTest(fun, tp2, flagUnrelated))
            }
          case defn.MultiArrayOf(elem, ndims) if isUnboundedGeneric(elem) =>
            def isArrayTest(arg: Tree) =
              ref(defn.runtimeMethodRef(nme.isArray)).appliedTo(arg, Literal(Constant(ndims)))
            if (ndims == 1) isArrayTest(qual)
            else evalOnce(qual) { qual1 =>
              derivedTree(qual1, defn.Any_isInstanceOf, qual1.tpe) and isArrayTest(qual1)
            }
          case _ =>
            transformIsInstanceOf(qual, erasure(testType), flagUnrelated)
        }

        if ((sym eq defn.Any_isInstanceOf) || (sym eq defn.Any_typeTest))
          transformTypeTest(qual, tree.args.head.tpe, flagUnrelated = true)
        else if (sym eq defn.Any_asInstanceOf)
          transformAsInstanceOf(erasure(tree.args.head.tpe))
        else tree

      case _ =>
        tree
    }
  }
}
