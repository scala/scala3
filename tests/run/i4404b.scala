// scalajs: --skip

import java.lang.reflect.Modifier

object Test {
  val x = {
    final class Foo
    new Foo
  }

  def main(args: Array[String]) = {
    val m = x.getClass.getModifiers
    println(Modifier.isPrivate(m))
    println(Modifier.isFinal(m))
  }
}
