import scala.collection.JavaConverters._

object Test {
  def test(x: Any): Unit = {
    x.asInstanceOf[java.util.List[_]].asScala.toList
  }
}
