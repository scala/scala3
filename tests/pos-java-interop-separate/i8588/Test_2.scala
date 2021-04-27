import scala.jdk.CollectionConverters.*
object Test {
  ConfigFactory_1.parseMap(Map("a" -> 1).asJava)
  ConfigFactory_1.parseMap(Map("a" -> "", "b" -> true).asJava)
}
