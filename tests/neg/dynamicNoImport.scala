class Foo extends scala.Dynamic // error
trait Bar extends scala.Dynamic // error
object Baz extends scala.Dynamic // error

package A {
  import scala.language.dynamics
  package B {
    import scala.language.{ dynamics => _ }
    class Foo extends scala.Dynamic // error
    trait Bar extends scala.Dynamic // error
    object Baz extends scala.Dynamic // error
  }
}
