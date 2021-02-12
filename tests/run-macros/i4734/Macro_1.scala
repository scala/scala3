import scala.annotation.tailrec
import scala.quoted.*

object Macros {
  inline def unrolledForeach(seq: IndexedSeq[Int], inline f: Int => Unit, inline unrollSize: Int): Unit = // or f: Int => Unit
    ${ unrolledForeachImpl('seq, 'f, 'unrollSize) }

  def unrolledForeachImpl(seq: Expr[IndexedSeq[Int]], f: Expr[Int => Unit], unrollSizeExpr: Expr[Int]) (using Quotes): Expr[Unit] =
    unrolledForeachImpl(seq, f, unrollSizeExpr.valueOrError)

  def unrolledForeachImpl(seq: Expr[IndexedSeq[Int]], f: Expr[Int => Unit], unrollSize: Int)(using Quotes): Expr[Unit] = '{
    val size = ($seq).length
    assert(size % (${Expr(unrollSize)}) == 0) // for simplicity of the implementation
    var i = 0
    while (i < size) {
      ${
        for (j <- new UnrolledRange(0, unrollSize)) '{
          val index = i + ${Expr(j)}
          val element = ($seq)(index)
          ${ Expr.betaReduce('{$f(element)}) } // or `($f)(element)` if `f` should not be inlined
        }
      }
      i += ${Expr(unrollSize)}
    }

  }

  class UnrolledRange(start: Int, end: Int) {
    def foreach(f: Int => Expr[Unit])(using Quotes): Expr[Unit] = {
      @tailrec def loop(i: Int, acc: Expr[Unit]): Expr[Unit] =
        if (i >= 0) loop(i - 1, '{ ${f(i)}; $acc })
        else acc
      loop(end - 1, '{})
    }
  }
}
