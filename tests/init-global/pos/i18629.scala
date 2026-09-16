import scala.language.unsafeNulls
object Foo {
  val bar = List() match {
    case List() => ???
    case null => ???
  }
}
