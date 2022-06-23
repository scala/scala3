// scalajs: --skip

import java.lang.reflect.Modifier

trait T {
  def f = {
    def f0 = ()
  }
}

class C {
  def g = {
    def g0 = ()
  }
}

object Test {
  def main(args: Array[String]) = {
    List((new T {}).getClass.getInterfaces.head, (new C).getClass)
      .map(_.getDeclaredMethods)
      .foreach { ms =>
        println(ms.exists(m => Modifier.isFinal(m.getModifiers)))
      }
  }
}
