package dotty.tools.dotc
package transform

import core._
import Contexts._, Symbols._, Types._, Constants._, StdNames._, Decorators._
import ast.Trees._
import Erasure.Boxing._
import TypeErasure._
import ValueClasses._
import SymUtils._
import core.Flags._
import util.Positions._
import reporting.trace


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
object TypeTestsCasts {
  import ast.tpd._

  def interceptTypeApply(tree: TypeApply)(implicit ctx: Context): Tree = trace(s"transforming ${tree.show}", show = true) {
    tree.fun match {
      case fun @ Select(expr, selector) =>
        val sym = tree.symbol

        def isPrimitive(tp: Type) = tp.classSymbol.isPrimitiveValueClass

        def derivedTree(expr1: Tree, sym: Symbol, tp: Type) =
          cpy.TypeApply(tree)(expr1.select(sym).withPos(expr.pos), List(TypeTree(tp)))

        def foundCls = expr.tpe.widen.classSymbol
        // println(i"ta $tree, found = $foundCls")

        def inMatch =
          fun.symbol == defn.Any_typeTest ||  // new scheme
          expr.symbol.is(Case)                // old scheme

        def transformIsInstanceOf(expr:Tree, testType: Type, flagUnrelated: Boolean): Tree = {
          def testCls = testType.classSymbol

          def unreachable(why: => String) =
            if (flagUnrelated)
              if (inMatch) ctx.error(em"this case is unreachable since $why", expr.pos)
              else ctx.warning(em"this will always yield false since $why", expr.pos)

          /** Are `foundCls` and `testCls` classes that allow checks
           *  whether a test would be always false?
           */
          def isCheckable =
            foundCls.isClass && testCls.isClass &&
            !(testCls.isPrimitiveValueClass && !foundCls.isPrimitiveValueClass) &&
               // if `test` is primitive but `found` is not, we might have a case like
               // found = java.lang.Integer, test = Int, which could be true
               // (not sure why that is so, but scalac behaves the same way)
            !isDerivedValueClass(foundCls) && !isDerivedValueClass(testCls)
               // we don't have the logic to handle derived value classes

          /** Check whether a runtime test that a value of `foundCls` can be a `testCls`
           *  can be true in some cases. Issure a warning or an error if that's not the case.
           */
          def checkSensical: Boolean =
            if (!isCheckable) true
            else if (foundCls.isPrimitiveValueClass && !testCls.isPrimitiveValueClass) {
                ctx.error("cannot test if value types are references", tree.pos)
                false
              }
            else if (!foundCls.derivesFrom(testCls)) {
              if (foundCls.is(Final)) {
                unreachable(i"$foundCls is not a subclass of $testCls")
                false
              }
              else if (!testCls.derivesFrom(foundCls) &&
                       (testCls.is(Final) ||
                        !testCls.is(Trait) && !foundCls.is(Trait))) {
                unreachable(i"$foundCls and $testCls are unrelated")
                false
              }
              else true
            }
            else true

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
          else
            derivedTree(expr, defn.Any_isInstanceOf, testType)
        }

        def transformAsInstanceOf(testType: Type): Tree = {
          def testCls = testType.widen.classSymbol
          if (expr.tpe <:< testType)
            Typed(expr, tree.args.head)
          else if (foundCls.isPrimitiveValueClass) {
            if (testCls.isPrimitiveValueClass) primitiveConversion(expr, testCls)
            else derivedTree(box(expr), defn.Any_asInstanceOf, testType)
          }
          else if (testCls.isPrimitiveValueClass)
            unbox(expr.ensureConforms(defn.ObjectType), testType)
          else if (isDerivedValueClass(testCls)) {
            expr // adaptToType in Erasure will do the necessary type adaptation
          }
          else
            derivedTree(expr, defn.Any_asInstanceOf, testType)
        }

        /** Transform isInstanceOf OrType
         *
         *    expr.isInstanceOf[A | B]  ~~>  expr.isInstanceOf[A] | expr.isInstanceOf[B]
         *    expr.isInstanceOf[A & B]  ~~>  expr.isInstanceOf[A] & expr.isInstanceOf[B]
         *
         *  The transform happens before erasure of `testType`, thus cannot be merged
         *  with `transformIsInstanceOf`, which depends on erased type of `testType`.
         */
        def transformTypeTest(expr: Tree, testType: Type, flagUnrelated: Boolean): Tree = testType.dealias match {
          case _: SingletonType =>
            expr.isInstance(testType).withPos(tree.pos)
          case OrType(tp1, tp2) =>
            evalOnce(expr) { e =>
              transformTypeTest(e, tp1, flagUnrelated = false)
                .or(transformTypeTest(e, tp2, flagUnrelated = false))
            }
          case AndType(tp1, tp2) =>
            evalOnce(expr) { e =>
              transformTypeTest(e, tp1, flagUnrelated)
                .and(transformTypeTest(e, tp2, flagUnrelated))
            }
          case defn.MultiArrayOf(elem, ndims) if isUnboundedGeneric(elem) =>
            def isArrayTest(arg: Tree) =
              ref(defn.runtimeMethodRef(nme.isArray)).appliedTo(arg, Literal(Constant(ndims)))
            if (ndims == 1) isArrayTest(expr)
            else evalOnce(expr) { e =>
              derivedTree(e, defn.Any_isInstanceOf, e.tpe)
                .and(isArrayTest(e))
            }
          case _ =>
            transformIsInstanceOf(expr, erasure(testType), flagUnrelated)
        }

        if (sym.isTypeTest)
          transformTypeTest(expr, tree.args.head.tpe, flagUnrelated = true)
        else if (sym eq defn.Any_asInstanceOf)
          transformAsInstanceOf(erasure(tree.args.head.tpe))
        else tree

      case _ =>
        tree
    }
  }
}
