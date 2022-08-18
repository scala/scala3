// scalajs: --skip

import java.lang.reflect.Modifier

class C[A](private val a: Any) extends AnyVal

object Test {
  val f = (x: C[Any]) => {println(s"f($x)"); x}
  def main(args: Array[String]): Unit = {
     f(new C("."))
     val methods = f.getClass.getDeclaredMethods.filter(m => (m.getModifiers & Modifier.PRIVATE) == 0).map(_.getName).sorted
     println("")
     println(methods.mkString("\n"))
  }
}

