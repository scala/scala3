
package foo {
  import scala.language.dynamics
  class Bar extends scala.Dynamic
}

class Baz extends foo.Bar
