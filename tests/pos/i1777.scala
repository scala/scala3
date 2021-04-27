object Main extends App {
  import scala.collection.immutable.*
  case class Foo(s: String)
  {
    implicit val orderingS: Ordering[String] = Ordering[String] // Crash
    val tree = TreeMap.empty ++ (1 to 100).map { i => Foo(i.toString) -> i }
    println(tree.getClass)
  }
}
