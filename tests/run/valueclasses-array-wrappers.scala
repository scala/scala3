import scala.collection.mutable._

class X(val a: Int) extends AnyVal {
  override def toString = s"X-$a"
}
class Y(val y: String) extends AnyVal
class A
object Test {
  def prettyPrintArray(x: Array[_]) = println("Array(" + x.mkString(", ") + ")")
  def prettyPrintArray2(x: Array[X]) = {
    val x0 = x(0)
    x.toString
    println(x.mkString(", "))
  }
  def prettyPrintArray3(x: Array[_]) = { println(x.toString); println(x(0)) }

  def main(args: Array[String]): Unit = {
    prettyPrintArray(test)
    prettyPrintArray(Array(new X(5)))

    prettyPrintArray(Array() :+ 1)
    prettyPrintArray2(test)

    println(test3)
    val a = test3(0)

    println(test4)
    prettyPrintArray3(test)

    test10
    test11(Array(1,2,3))

    val b: ArrayOps[X] = Array(new X(11))
    println(s"${b.asInstanceOf[dotty.runtime.vc.VCArrayOps[_]].vcElementClass}")
    val c: ArrayOps[Y] = Array(new Y("yyy"))
    println(s"${c.asInstanceOf[dotty.runtime.vc.VCArrayOps[_]].vcElementClass}")
  }

  def test: Array[X] = Array(new X(7), new X(9), new X(11))
  def test2: WrappedArray[X] = test
  def test3: Array[X] = Array() :+ (new X(7))
  def test4: Array[X] = Array()
  def test9: Array[Int] = Array(1,2,3)
  def test10: ArrayOps[Int] = test9
  def test11(x: Array[_]): ArrayOps[_] = x
}