// scalajs: --skip

class ValueClass(val i: Integer) extends AnyVal
class SomeClass

object Test {
  def main(args: Array[String]): Unit = {
    val cls: Predef.Class[SomeClass] = classOf[SomeClass]
    val valCls: Predef.Class[ValueClass] = classOf[ValueClass]
    val iCls: Class[Int] = classOf[Int]

    println("Value types:")
    println(classOf[Unit])
    println(classOf[Boolean])
    println(classOf[Byte])
    println(classOf[Short])
    println(classOf[Char])
    println(classOf[Int])
    println(classOf[Long])
    println(classOf[Float])
    println(classOf[Double])

    println("\nClass types:")
    println(classOf[SomeClass])
    println(classOf[ValueClass])
    println(classOf[List[Array[Float]]])
    println(classOf[(String, Map[Int, String])])

    println("\nArrays:")
    println(classOf[Array[Unit]])
    println(classOf[Array[Int]])
    println(classOf[Array[Double]])
    println(classOf[Array[List[String]]])

    println("\nFunctions:")
    println(classOf[(Int, Int) => Unit])
    println(classOf[Int => Boolean])
  }
}
