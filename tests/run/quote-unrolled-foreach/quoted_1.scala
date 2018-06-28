import scala.annotation.tailrec
import scala.quoted._

object Macro {

  inline def unrolledForeach(inline unrollSize: Int, seq: Array[Int])(f: => Int => Unit): Unit = // or f: Int => Unit
    ~unrolledForeachImpl(unrollSize, '(seq), '(f))

  private def unrolledForeachImpl(unrollSize: Int, seq: Expr[Array[Int]], f: Expr[Int => Unit]): Expr[Unit] = '{
    val size = (~seq).length
    assert(size % (~unrollSize.toExpr) == 0) // for simplicity of the implementation
    var i = 0
    while (i < size) {
      println("<log> start loop")
      ~{
        for (j <- new UnrolledRange(0, unrollSize)) '{
          val element = (~seq)(i + ~j.toExpr)
          ~f('(element)) // or `(~f)(element)` if `f` should not be inlined
        }
      }
      i += ~unrollSize.toExpr


    }

  }


  def foreach1(arrRef: Expr[Array[Int]], f: Expr[Int => Unit]): Expr[Unit] = '{
    val size = (~arrRef).length
    var i = 0
    while (i < size) {
      val element: Int = (~arrRef)(i)
      (~f)(element)
      i += 1
    }
  }

  def foreach1Tpe1[T](arrRef: Expr[Array[T]], f: Expr[T => Unit])(implicit t: Type[T]): Expr[Unit] = '{
    val size = (~arrRef).length
    var i = 0
    while (i < size) {
      val element: ~t = (~arrRef)(i)
      (~f)(element)
      i += 1
    }
  }

  def foreach1Tpe2[T: Type](arrRef: Expr[Array[T]], f: Expr[T => Unit]): Expr[Unit] = '{
    val size = (~arrRef).length
    var i = 0
    while (i < size) {
      val element: T = (~arrRef)(i)
      (~f)(element)
      i += 1
    }
  }

  def foreach2(arrRef: Expr[Array[Int]], f: Expr[Int => Unit]): Expr[Unit] = '{
    val size = (~arrRef).length
    var i = 0
    while (i < size) {
      val element = (~arrRef)(i)
      ~f('(element)) // Use AppliedFuntion
      i += 1
    }
  }

  def foreach3(arrRef: Expr[Array[Int]], f: Expr[Int => Unit]): Expr[Unit] = '{
    val size = (~arrRef).length
    var i = 0
    assert(size % 3 == 0) // for simplicity of the implementation
    while (i < size) {
      val element1 = (~arrRef)(i)
      ~f('(element1))
      val element2 = (~arrRef)(i + 1)
      ~f('(element2))
      val element3 = (~arrRef)(i + 3)
      ~f('(element3))
      i += 3
    }
  }

  def foreach4(arrRef: Expr[Array[Int]], f: Expr[Int => Unit], unrollSize: Int): Expr[Unit] = '{
    val size = (~arrRef).length
    var i = 0
    assert(size % ~unrollSize.toExpr == 0) // for simplicity of the implementation
    while (i < size) {
      ~{
        @tailrec def loop(j: Int, acc: Expr[Unit]): Expr[Unit] =
          if (j >= 0) loop(j - 1, '{ (~f)(i + ~j.toExpr); ~acc })
          else acc
        loop(unrollSize - 1, '())
      }
      i += ~unrollSize.toExpr
    }
  }

  private class UnrolledRange(start: Int, end: Int) {
    def foreach(f: Int => Expr[Unit]): Expr[Unit] = {
      @tailrec def loop(i: Int, acc: Expr[Unit]): Expr[Unit] =
        if (i >= 0) loop(i - 1, '{ ~f(i); ~acc })
        else acc
      loop(end - 1, '())
    }
  }
}
