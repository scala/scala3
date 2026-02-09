import scala.annotation.{experimental,unroll}

@experimental final class Foo {
  def bar(@unroll x: Int = 0) = x + 1
}