import SpecializeUtils._

object Test {
  def main(args: Array[String]): Unit = {
    val foo = new Foo
    System.out.println(foo.fun1(new Bar))
    System.out.println(foo.fun2(new Bar))

    checkMethodExists(classOf[Foo], "fun1", List(classOf[Foo]), classOf[Foo], specialized = false)
    checkMethodExists(classOf[Foo], "fun1", List(classOf[Bar]), classOf[Bar])

    checkMethodExists(classOf[Foo], "fun2", List(classOf[Foo]), classOf[Foo], specialized = false)
  }

}

class Foo {
  def fun1[F <: Foo](x: F) = x // Specialized
  def fun2(x: Foo) = x // Not specialized

}

class Bar extends Foo {
  override def toString: String = "Bar"
}

object SpecializeUtils {

  def checkMethodExists(cls: Class[_], name: String, params: List[Class[_]], ret: Class[_], specialized: Boolean = true): Unit = {
    val nameMatch = if (specialized) name + "\\$spec\\d*" else name
    val methods = cls.getDeclaredMethods
    assert(methods.count(x => x.getName.matches(nameMatch) && x.getParameterTypes.toList == params && x.getReturnType == ret) == 1)
  }

}
