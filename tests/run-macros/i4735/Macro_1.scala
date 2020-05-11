import scala.annotation.tailrec

import scala.quoted._

object Macro {

  inline def unrolledForeach(inline unrollSize: Int, seq: Array[Int], inline f: Int => Unit): Unit = // or f: Int => Unit
    ${ unrolledForeachImpl('unrollSize, 'seq, 'f) }

  private def unrolledForeachImpl(using s: Scope)(unrollSize: s.Expr[Int], seq: s.Expr[Array[Int]], f: s.Expr[Int => Unit]): s.Expr[Unit] = '{
    val size = ($seq).length
    assert(size % (${unrollSize}) == 0) // for simplicity of the implementation
    var i = 0
    while (i < size) {
      println("<log> start loop")
      ${
        for (j <- new UnrolledRange(0, unrollSize.unliftOrError)) '{
          val element = ($seq)(i + ${Expr(j)})
          ${Expr.betaReduce('{$f(element)})} // or `($f)(element)` if `f` should not be inlined
        }
      }
      i += ${unrollSize}
    }

  }

  private class UnrolledRange(start: Int, end: Int) {
    def foreach(using s: Scope)(f: Int => s.Expr[Unit]): s.Expr[Unit] = {
      @tailrec def loop(i: Int, acc: s.Expr[Unit]): s.Expr[Unit] =
        if (i >= 0) loop(i - 1, '{ ${f(i)}; $acc })
        else acc
      loop(end - 1, '{})
    }
  }
}
