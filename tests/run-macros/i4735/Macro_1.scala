import scala.annotation.tailrec
import scala.quoted.autolift.{given _}

import scala.quoted._

object Macro {

  inline def unrolledForeach(inline unrollSize: Int, seq: Array[Int], f: => Int => Unit): Unit = // or f: Int => Unit
    ${ unrolledForeachImpl('unrollSize, 'seq, 'f) }

  private def unrolledForeachImpl(unrollSize: Expr[Int], seq: Expr[Array[Int]], f: Expr[Int => Unit]) with QuoteContext : Expr[Unit] = '{
    val size = ($seq).length
    assert(size % (${unrollSize}) == 0) // for simplicity of the implementation
    var i = 0
    while (i < size) {
      println("<log> start loop")
      ${
        for (j <- new UnrolledRange(0, unrollSize.value)) '{
          val element = ($seq)(i + ${j})
          ${Expr.betaReduce(f)('element)} // or `($f)(element)` if `f` should not be inlined
        }
      }
      i += ${unrollSize}
    }

  }

  private class UnrolledRange(start: Int, end: Int) {
    def foreach(f: Int => Expr[Unit]) with QuoteContext : Expr[Unit] = {
      @tailrec def loop(i: Int, acc: Expr[Unit]): Expr[Unit] =
        if (i >= 0) loop(i - 1, '{ ${f(i)}; $acc })
        else acc
      loop(end - 1, '{})
    }
  }
}
