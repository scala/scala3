class C[A](private val a: Any) extends AnyVal

object Test {
  val f = (x: C[Any]) => {println(s"f($x)"); x}
  def main(args: Array[String]): Unit = {
     f(new C("."))
     val methods = f.getClass.getDeclaredMethods.map(_.nn.getName.nn).sorted
     println("")
     println(methods.mkString("\n"))
  }
}

