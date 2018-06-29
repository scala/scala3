import scala.annotation.tailrec
import scala.quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make

  def main(args: Array[String]): Unit = {
    val code1 = '{ (arr: Array[Int], f: Int => Unit) => ~foreach1('(arr), '(f)) }
    println(code1.show)
    println()

    val code1Tpe = '{ (arr: Array[String], f: String => Unit) => ~foreach1Tpe1('(arr), '(f)) }
    println(code1Tpe.show)
    println()

    val code1Tpe2 = '{ (arr: Array[String], f: String => Unit) => ~foreach1Tpe1('(arr), '(f)) }
    println(code1Tpe2.show)
    println()

    val code2 = '{ (arr: Array[Int]) => ~foreach1('(arr), '(i => System.out.println(i))) }
    println(code2.show)
    println()

    val code3 = '{ (arr: Array[Int], f: Int => Unit) => ~foreach3('(arr), '(f)) }
    println(code3.show)
    println()

    val code4 = '{ (arr: Array[Int], f: Int => Unit) => ~foreach4('(arr), '(f), 4) }
    println(code4.show)
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
    if (size % 3 != 0) throw new Exception("...")// for simplicity of the implementation
    while (i < size) {
      (~f)((~arrRef)(i))
      (~f)((~arrRef)(i + 1))
      (~f)((~arrRef)(i + 2))
      i += 3
    }
  }

  def foreach4(arrRef: Expr[Array[Int]], f: Expr[Int => Unit], unrollSize: Int): Expr[Unit] = '{
    val size = (~arrRef).length
    var i = 0
    if (size % ~unrollSize.toExpr != 0) throw new Exception("...") // for simplicity of the implementation
    while (i < size) {
      ~{
        @tailrec def loop(j: Int, acc: Expr[Unit]): Expr[Unit] =
          if (j >= 0) loop(j - 1, '{ (~f)((~arrRef)(i + ~j.toExpr)); ~acc })
        else acc
        loop(unrollSize - 1, '())
      }
      i += ~unrollSize.toExpr
    }
  }

}
