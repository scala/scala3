import scala.annotation.tailrec
import scala.quoted._
import scala.quoted.autolift._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    def code1 given QuoteContext = '{ (arr: Array[Int], f: Int => Unit) => ${ foreach1('arr, 'f) } }
    println(code1.show)
    println()

    def code1Tpe given QuoteContext = '{ (arr: Array[String], f: String => Unit) => ${ foreach1Tpe1('arr, 'f) } }
    println(code1Tpe.show)
    println()

    def code1Tpe2 given QuoteContext = '{ (arr: Array[String], f: String => Unit) => ${ foreach1Tpe1('arr, 'f) } }
    println(code1Tpe2.show)
    println()

    def code2 given QuoteContext = '{ (arr: Array[Int]) => ${ foreach1('arr, '{i => System.out.println(i)}) } }
    println(code2.show)
    println()

    def code3 given QuoteContext = '{ (arr: Array[Int], f: Int => Unit) => ${ foreach3('arr, 'f) } }
    println(code3.show)
    println()

    def code4 given QuoteContext = '{ (arr: Array[Int], f: Int => Unit) => ${ foreach4('arr, 'f, 4) } }
    println(code4.show)
    println()

    def liftedArray given QuoteContext: Expr[Array[Int]] = Array(1, 2, 3, 4)
    println(liftedArray.show)
    println()


    def printAll(arr: Array[Int]) = '{
      val arr1 = ${ arr }
      ${ foreach1('arr1, '{x => println(x)}) }
    }

    println(printAll(Array(1, 3, 4, 5)).show)
    '{}
  }

  def foreach1(arrRef: Expr[Array[Int]], f: Expr[Int => Unit]) given QuoteContext: Expr[Unit] = '{
    val size = ($arrRef).length
    var i = 0
    while (i < size) {
      val element: Int = ($arrRef)(i)
      ($f)(element)
      i += 1
    }
  }

  def foreach1Tpe1[T](arrRef: Expr[Array[T]], f: Expr[T => Unit])(implicit t: Type[T], qctx: QuoteContext): Expr[Unit] = '{
    val size = ($arrRef).length
    var i = 0
    while (i < size) {
      val element: $t = ($arrRef)(i)
      ($f)(element)
      i += 1
    }
  }

  def foreach1Tpe2[T: Type](arrRef: Expr[Array[T]], f: Expr[T => Unit]) given QuoteContext: Expr[Unit] = '{
    val size = ($arrRef).length
    var i = 0
    while (i < size) {
      val element: T = ($arrRef)(i)
      ($f)(element)
      i += 1
    }
  }

  def foreach2(arrRef: Expr[Array[Int]], f: Expr[Int => Unit]) given QuoteContext: Expr[Unit] = '{
    val size = ($arrRef).length
    var i = 0
    while (i < size) {
      val element = ($arrRef)(i)
      ${ f('element) } // Use AppliedFuntion
      i += 1
    }
  }

  def foreach3(arrRef: Expr[Array[Int]], f: Expr[Int => Unit]) given QuoteContext: Expr[Unit] = '{
    val size = ($arrRef).length
    var i = 0
    if (size % 3 != 0) throw new Exception("...")// for simplicity of the implementation
    while (i < size) {
      ($f)(($arrRef)(i))
      ($f)(($arrRef)(i + 1))
      ($f)(($arrRef)(i + 2))
      i += 3
    }
  }

  def foreach3_2(arrRef: Expr[Array[Int]], f: Expr[Int => Unit]) given QuoteContext: Expr[Unit] = '{
    val size = ($arrRef).length
    var i = 0
    if (size % 3 != 0) throw new Exception("...")// for simplicity of the implementation
    while (i < size) {
      ($f)(($arrRef)(i))
      ($f)(($arrRef)(i + 1))
      ($f)(($arrRef)(i + 2))
      i += 3
    }
  }

  def foreach4(arrRef: Expr[Array[Int]], f: Expr[Int => Unit], unrollSize: Int) given QuoteContext: Expr[Unit] = '{
    val size = ($arrRef).length
    var i = 0
    if (size % ${unrollSize} != 0) throw new Exception("...") // for simplicity of the implementation
    while (i < size) {
      ${ foreachInRange(0, unrollSize)(j => '{ ($f)(($arrRef)(i + ${j})) }) }
      i += ${unrollSize}
    }
  }

  implicit object ArrayIntIsLiftable extends Liftable[Array[Int]] {
    override def toExpr(x: Array[Int]) given QuoteContext: Expr[Array[Int]] = '{
      val array = new Array[Int](${x.length})
      ${ foreachInRange(0, x.length)(i => '{ array(${i}) = ${x(i)}}) }
      array
    }
  }

  def foreachInRange(start: Int, end: Int)(f: Int => Expr[Unit]) given QuoteContext: Expr[Unit] = {
    @tailrec def unroll(i: Int, acc: Expr[Unit]): Expr[Unit] =
      if (i < end) unroll(i + 1, '{ $acc; ${f(i)} }) else acc
    if (start < end) unroll(start + 1, f(start)) else '{}
  }

}
