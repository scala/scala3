class A1[X](val x: X)
class A2[F[_]](val x: F[String])

object Test {
  def test(clazz: Class[?]): Unit = {
    val List(constructor) = clazz.getConstructors().toList
    println(constructor.getTypeParameters().length)
    println(constructor.getTypeParameters().mkString(", "))
    println(constructor.toGenericString())
  }

  def main(args: Array[String]): Unit = {
    test(classOf[A1[?]])
    test(classOf[A2[?]])
  }
}
