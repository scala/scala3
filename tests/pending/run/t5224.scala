import scala.reflect.runtime.universe._

class Foo(bar: String) extends annotation.ClassfileAnnotation

object Test extends dotty.runtime.LegacyApp {
  val tree = reify{@Foo(bar = "qwe") class C}.tree
  println(tree.toString)
}
