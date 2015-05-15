import scala.reflect.runtime.universe._

class Foo extends annotation.StaticAnnotation

object Test extends dotty.runtime.LegacyApp {
  val tree = reify{(5: @Foo).asInstanceOf[Int]}.tree
  println(tree.toString)
}
