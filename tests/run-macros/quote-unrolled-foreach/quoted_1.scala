import scala.annotation.tailrec
import scala.quoted._
import scala.quoted.autolift.{given _}

object Macro {

  inline def unrolledForeach(inline unrollSize: Int, seq: Array[Int])(f: => Int => Unit): Unit = // or f: Int => Unit
    ${unrolledForeachImpl('unrollSize, 'seq, 'f)}

  private def unrolledForeachImpl(unrollSizeExpr: Expr[Int], seq: Expr[Array[Int]], f: Expr[Int => Unit]) with QuoteContext : Expr[Unit] =
    unrolledForeachImpl(unrollSizeExpr.value, seq, f)

  private def unrolledForeachImpl(unrollSize: Int, seq: Expr[Array[Int]], f: Expr[Int => Unit]) with QuoteContext : Expr[Unit] = '{
    val size = $seq.length
    assert(size % (${unrollSize}) == 0) // for simplicity of the implementation
    var i = 0
    while (i < size) {
      println("<log> start loop")
      ${
        @tailrec def loop(j: Int, acc: Expr[Unit]): Expr[Unit] =
        if (j >= 0) loop(j - 1, '{ ${Expr.betaReduce(f)('{$seq(i + ${j})})}; $acc })
        else acc
        loop(unrollSize - 1, '{})
      }
      i += ${unrollSize}
    }
  }
}
