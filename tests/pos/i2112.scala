import scala.jdk.CollectionConverters.*

object Test {
  def test(x: Any): Unit = {
    x.asInstanceOf[java.util.List[_]].asScala.toList
  }
}
