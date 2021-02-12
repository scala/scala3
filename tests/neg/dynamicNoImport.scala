class Foo extends scala.Dynamic // error
trait Bar extends scala.Dynamic // error
object Baz extends scala.Dynamic // error

package A {
  import scala.language.dynamics
  package B {
    import scala.language.dynamics as _
    class Foo extends scala.Dynamic // error
    trait Bar extends scala.Dynamic // error
    object Baz extends scala.Dynamic // error

    package C {
      import scala.language.dynamics as d
      class Foo extends scala.Dynamic // OK
    }
  }
}
