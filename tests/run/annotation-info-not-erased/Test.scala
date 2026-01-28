// scalajs: --skip
import scala.annotation.meta.getter

class C {
  type Rg = Resource @getter
  @(Resource @getter)(`type` = classOf[Int]) def a = 0
  @Rg(`type` = classOf[Int])                 def b = 0
}

object Test {
  def typeArg(meth: String) =
    classOf[C]
      .getDeclaredMethod(meth)
      .getDeclaredAnnotation(classOf[Resource]).`type`

  def main(args: Array[String]): Unit = {
    val result = List("a", "b").map(typeArg)
    assert(result == List(Integer.TYPE, Integer.TYPE))
  }
}
