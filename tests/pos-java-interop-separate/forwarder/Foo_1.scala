import java.util.List

trait Foo {
  val x: List[String] = null
}
abstract class Bar extends Foo

