import scala.annotation.tailrec
import scala.quoted.autolift._

import scala.quoted._

object Macro {

  inline def unrolledForeach(inline unrollSize: Int, seq: Array[Int], f: => Int => Unit): Unit = // or f: Int => Unit
    ${ unrolledForeachImpl(unrollSize, 'seq, 'f) }

  // FIXME add private issue #5295
  /*private*/ def unrolledForeachImpl(unrollSize: Int, seq: Expr[Array[Int]], f: Expr[Int => Unit]): Staged[Unit] = '{
    val size = ($seq).length
    assert(size % (${unrollSize}) == 0) // for simplicity of the implementation
    var i = 0
    while (i < size) {
      println("<log> start loop")
      ${
        for (j <- new UnrolledRange(0, unrollSize)) '{
          val element = ($seq)(i + ${j})
          ${f('element)} // or `($f)(element)` if `f` should not be inlined
        }
      }
      i += ${unrollSize}
    }

  }

  // FIXME add private issue #5295
  /*private*/ class UnrolledRange(start: Int, end: Int) {
    def foreach(f: Int => Expr[Unit]): Staged[Unit] = {
      @tailrec def loop(i: Int, acc: Expr[Unit]): Expr[Unit] =
        if (i >= 0) loop(i - 1, '{ ${f(i)}; $acc })
      else acc
      loop(end - 1, '{})
    }
  }
}
