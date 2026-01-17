//> using options -preview
import scala.annotation.unroll

final class Foo {
  def bar(@unroll x: Int = 0) = x + 1
}
