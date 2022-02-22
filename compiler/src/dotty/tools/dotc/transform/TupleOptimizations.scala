package dotty.tools.dotc
package transform

import core._
import Contexts._
import Decorators._
import Definitions._
import DenotTransformers._
import StdNames._
import Symbols._
import MegaPhase._
import Types._
import dotty.tools.dotc.ast.tpd


/** Optimize generic operations on tuples */
class TupleOptimizations extends MiniPhase with IdentityDenotTransformer {
  import tpd._

  override def phaseName: String = TupleOptimizations.name

  override def description: String = TupleOptimizations.description

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    if (!tree.symbol.exists || tree.symbol.owner != defn.RuntimeTuplesModuleClass) tree
    else if (tree.symbol == defn.RuntimeTuples_cons) transformTupleCons(tree)
    else if (tree.symbol == defn.RuntimeTuples_tail) transformTupleTail(tree)
    else if (tree.symbol == defn.RuntimeTuples_size) transformTupleSize(tree)
    else if (tree.symbol == defn.RuntimeTuples_concat) transformTupleConcat(tree)
    else if (tree.symbol == defn.RuntimeTuples_apply) transformTupleApply(tree)
    else if (tree.symbol == defn.RuntimeTuples_toArray) transformTupleToArray(tree)
    else tree

  private def transformTupleCons(tree: tpd.Apply)(using Context): Tree = {
    val head :: tail :: Nil = tree.args
    defn.tupleTypes(tree.tpe.widenTermRefExpr.dealias) match {
      case Some(tpes) =>
        // Generate a the tuple directly with TupleN+1.apply
        val size = tpes.size
        if (size <= 5)
          // val t = tail
          // TupleN+1(head, t._1, ..., t._n)
          evalOnce(Typed(tail, TypeTree(defn.tupleType(tpes.tail)))) { tup =>
            val elements = head :: tupleSelectors(tup, size - 1)
            knownTupleFromElements(tpes, elements)
          }
        else {
          // val it = Iterator.single(head) ++ tail.asInstanceOf[Product].productIterator
          // TupleN+1(it.next(), ..., it.next())
          val fullIterator = ref(defn.RuntimeTuples_consIterator).appliedToTermArgs(head :: tail :: Nil)
          evalOnce(fullIterator) { it =>
            knownTupleFromIterator(tpes.length, it).asInstance(tree.tpe)
          }
        }
      case _ =>
        // No optimization, keep:
        // scala.runtime.Tuples.cons(tail, head)
        tree
    }
  }

  private def transformTupleTail(tree: tpd.Apply)(using Context): Tree = {
    val Apply(_, tup :: Nil) = tree
    defn.tupleTypes(tup.tpe.widenTermRefExpr.dealias, MaxTupleArity + 1) match {
      case Some(tpes) =>
        // Generate a the tuple directly with TupleN-1.apply
        val size = tpes.size
        assert(size > 0)
        if (size == 1)
          // scala.EmptyTuple
          ref(defn.EmptyTupleModule.termRef)
        else if (size <= 5)
          // val t = tup.asInstanceOf[TupleN[...]]
          // TupleN-1(t._2, ..., t._n)
          evalOnce(Typed(tup, TypeTree(defn.tupleType(tpes)))) { tup =>
            val elements = tupleSelectors(tup, size).tail
            knownTupleFromElements(tpes.tail, elements)
          }
        else if (size <= MaxTupleArity + 1)
          // val it = this.asInstanceOf[Product].productIterator
          // it.next()
          // TupleN-1(it.next(), ..., it.next())
          evalOnce(tup.asInstance(defn.ProductClass.typeRef).select(nme.productIterator)) { it =>
            Block(
              it.select(nme.next).ensureApplied :: Nil,
              knownTupleFromIterator(size - 1, it).asInstance(tree.tpe)
            )
          }
        else
          // tup.asInstanceOf[TupleXXL].tailXXL
          tup.asInstance(defn.TupleXXLClass.typeRef).select("tailXXL".toTermName)
      case None =>
        // No optimization, keep:
        // scala.runtime.Tuples.tail(tup)
        tree
    }
  }

  private def transformTupleSize(tree: tpd.Apply)(using Context): Tree =
    tree.tpe.tryNormalize match {
      case tp: ConstantType => Literal(tp.value)
      case _ => tree
    }

  private def transformTupleConcat(tree: tpd.Apply)(using Context): Tree = {
    val Apply(_, self :: that :: Nil) = tree
    (defn.tupleTypes(self.tpe.widenTermRefExpr.dealias), defn.tupleTypes(that.tpe.widenTermRefExpr.dealias)) match {
      case (Some(tpes1), Some(tpes2)) =>
        // Generate a the tuple directly with TupleN+M.apply
        val n = tpes1.size
        val m = tpes2.size
        if (n == 0) that
        else if (m == 0) self
        else if (n + m < 5)
          // val t = self
          // val u = that
          // TupleN+M(t._1,..., t._N, u._1, ..., u._M)
          evalOnce(Typed(self, TypeTree(defn.tupleType(tpes1)))) { self =>
            evalOnce(Typed(that, TypeTree(defn.tupleType(tpes2)))) { that =>
              val types = tpes1 ::: tpes2
              val elements = tupleSelectors(self, n) ::: tupleSelectors(that, m)
              knownTupleFromElements(types, elements)
            }
          }
        else {
          // val it = self.asInstanceOf[Product].productIterator ++ that.asInstanceOf[Product].productIterator
          // TupleN+M(it.next(), ..., it.next())
          val fullIterator = ref(defn.RuntimeTuples_concatIterator).appliedToTermArgs(tree.args)
          evalOnce(fullIterator) { it =>
            knownTupleFromIterator(n + m, it).asInstance(tree.tpe)
          }
        }
      case _ =>
        // No optimization, keep:
        // scala.runtime.Tuples.cons(self, that)
        tree
    }
  }

  private def transformTupleApply(tree: tpd.Apply)(using Context): Tree = {
    val Apply(_, tup :: nTree :: Nil) = tree
    (defn.tupleTypes(tup.tpe.widenTermRefExpr.dealias), nTree.tpe) match {
      case (Some(tpes), nTpe: ConstantType) =>
        // Get the element directly with TupleM._n+1 or TupleXXL.productElement(n)
        val size = tpes.size
        val n = nTpe.value.intValue
        if (n < 0 || n >= size) {
          report.error("index out of bounds: " + n, nTree.underlyingArgument.srcPos)
          tree
        }
        else if (size <= MaxTupleArity)
          // tup._n
          Typed(tup, TypeTree(defn.tupleType(tpes))).select(nme.selectorName(n))
        else
          // tup.asInstanceOf[TupleXXL].productElement(n)
          tup.asInstance(defn.TupleXXLClass.typeRef).select(nme.productElement).appliedTo(Literal(nTpe.value))
      case (None, nTpe: ConstantType) if nTpe.value.intValue < 0 =>
        report.error("index out of bounds: " + nTpe.value.intValue, nTree.srcPos)
        tree
      case _ =>
        // No optimization, keep:
        // scala.runtime.Tuples.apply(tup, n)
        tree
    }
  }

  private def transformTupleToArray(tree: tpd.Apply)(using Context): Tree = {
    val Apply(_, tup :: Nil) = tree
    defn.tupleTypes(tup.tpe.widen, MaxTupleArity) match {
      case Some(tpes) =>
        val size = tpes.size
        if (size == 0)
          // Array.emptyObjectArray
          ref(defn.ArrayModule).select("emptyObjectArray".toTermName).ensureApplied.withSpan(tree.span)
        else if (size <= MaxTupleArity)
          // scala.runtime.Tuples.productToArray(tup.asInstanceOf[Product])
          ref(defn.RuntimeTuples_productToArray).appliedTo(tup.asInstance(defn.ProductClass.typeRef))
        else
          // tup.asInstanceOf[TupleXXL].elems.clone()
          tup.asInstance(defn.TupleXXLClass.typeRef).select(nme.toArray)
      case None =>
        // No optimization, keep:
        // scala.runtime.Tuples.toArray(tup)
        tree
    }
  }

  /** Create a TupleN (1 <= N < 23) from the elements */
  private def knownTupleFromElements(tpes: List[Type], elements: List[Tree])(using Context) = {
    val size = elements.size
    assert(0 < size && size <= MaxTupleArity)
    val tupleModule = defn.TupleType(size).classSymbol.companionModule
    ref(tupleModule).select(nme.apply).appliedToTypes(tpes).appliedToTermArgs(elements)
  }

  private def knownTupleFromIterator(size: Int, it: Tree)(using Context): Tree =
    if (size == 0)
      // EmptyTuple for empty tuple
      ref(defn.EmptyTupleModule.termRef) // TODO should this code be here? Or assert(size > specializedSize)
    else if (size <= MaxTupleArity) {
      // TupleN(it.next(), ..., it.next())

      // TODO outline this code for the 22 alternatives (or less, may not need the smallest ones)?
      // This would yield smaller bytecode at the cost of an extra (easily JIT inlinable) call.
      // def tupleN(it: Iterator[Any]): TupleN[Any, ..., Any] = Tuple(it.next(), ..., it.next())
      val tpes = List.fill(size)(defn.AnyType)
      val elements = (0 until size).map(_ => it.select(nme.next)).toList
      knownTupleFromElements(tpes, elements)
    }
    else
      // No optimization, keep:
      // TupleXXL.fromIterator(it)
      ref(defn.TupleXXL_fromIterator).appliedTo(it)

  private def tupleSelectors(tup: Tree, size: Int)(using Context): List[Tree] =
    (0 until size).map(i => tup.select(nme.selectorName(i))).toList
}

object TupleOptimizations:
  val name: String = "genericTuples"
  val description: String = "optimize generic operations on tuples"
