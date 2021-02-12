import collection.{mutable, concurrent}
import collection.JavaConverters.*
import java.util.concurrent.ConcurrentHashMap as CHM

object Bar {
  def assertType[T](t: T) = t
  val a = new CHM[String, String]().asScala += (("", ""))
  assertType[concurrent.Map[String, String]](a)
}
