// scalajs: --skip

class Foo(@Named s: String)

object Test {
  def main(args: Array[String]): Unit = {
    val ctor = classOf[Foo].getDeclaredConstructors()(0)
    val annots = ctor.getParameterAnnotations()(0)
    assert(annots.length == 1, annots.length)
  }
}
