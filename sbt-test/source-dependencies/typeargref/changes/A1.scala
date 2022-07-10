import scala.jdk.CollectionConverters._
import java.util.{ Map => JMap }

class A {
  // Inferred type for `param`: java.util.Map[Int, _ <: Int]#<parameter V of trait Map>
  def param = {
    val opt: Option[JMap[Int, _ <: Int]] = None
    opt.getOrElse(Map.empty.asJava).get(42)
  }
}
