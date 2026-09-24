import java.util.List

trait Foo {
  val x: List[String] = ???
}
abstract class Bar extends Foo

