object Test {
  def foo(a: Int | Double) = a match {
    case a: (Float | Boolean) => 1
    case _ => 0
  }

  def typeTest(a: Int | Double) = a.isInstanceOf[Float | Boolean] // false

  def typeCast(a: Int | Double) = a.asInstanceOf[Float | Boolean] // no error

  def main(args: Array[String]): Unit = {
    println(foo(4))

    println(typeTest(4))

    println(typeCast(5))

    Boolean.box(true) match {
     case a: (Float | Boolean) => println(1)
     case _ => println(0)
    }

    println(Boolean.box(true).isInstanceOf[Float | Boolean])

    println(Boolean.box(true).asInstanceOf[Float | Boolean])
  }
}
