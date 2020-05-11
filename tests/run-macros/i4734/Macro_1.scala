import scala.annotation.tailrec
import scala.quoted._

object Macros {
  inline def unrolledForeach(seq: IndexedSeq[Int], inline f: Int => Unit, inline unrollSize: Int): Unit = // or f: Int => Unit
    ${ unrolledForeachImpl0('seq, 'f, 'unrollSize) }

  def unrolledForeachImpl0(using s: Scope)(seq: s.Expr[IndexedSeq[Int]], f: s.Expr[Int => Unit], unrollSizeExpr: s.Expr[Int]): s.Expr[Unit] =
    unrolledForeachImpl(seq, f, unrollSizeExpr.unliftOrError)

  def unrolledForeachImpl(using s: Scope)(seq: s.Expr[IndexedSeq[Int]], f: s.Expr[Int => Unit], unrollSize: Int): s.Expr[Unit] = '{
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
    def foreach(using s: Scope)(f: Int => s.Expr[Unit]): s.Expr[Unit] = {
      @tailrec def loop(i: Int, acc: s.Expr[Unit]): s.Expr[Unit] =
        if (i >= 0) loop(i - 1, '{ ${f(i)}; $acc })
        else acc
      loop(end - 1, '{})
    }
  }
}
