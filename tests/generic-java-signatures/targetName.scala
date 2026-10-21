import scala.annotation.targetName

class Foo {
  @targetName("A")
  class X[T]

  @targetName("B")
  trait XT[T]

  @targetName("C")
  object XO

  def fooClass(a: X[Object]): X[String] = ???
  def fooTrait(a: XT[Object]): XT[String] = ???
  def fooObj(a: XO.type, b: X[String]): XO.type = ???
  def fooComplex(a: X[XT[XO.type]]): XT[X[String]] = ???
}

object Test {
  def main(args: Array[String]): Unit = {
    classOf[Foo].getMethods.filter(_.getName.startsWith("foo")).sortBy(_.getName).foreach(m =>
      println(m.getName)
      m.getGenericParameterTypes.foreach(println)
      println(m.getGenericReturnType)
      println("---")
    )
  }
}
