package scalapackage:
  object Foo extends javapackage.Base_1
end scalapackage

object Test {
  def main(args: Array[String]): Unit = {
    println(
      Class.forName("scalapackage.Foo").getMethods
        .filter(m => (m.getModifiers & java.lang.reflect.Modifier.STATIC) != 0)
        .mkString("\n"))
  }
}
