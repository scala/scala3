import scala.collection.JavaConverters.*

object Test {
  def test(x: Any): Unit = {
    x.asInstanceOf[java.util.List[_]].asScala.toList
  }
}
