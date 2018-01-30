import scala.annotation.dependent

trait Foo { type FA }
class Bar(@dependent val foo: Foo) {
  type FA = foo.FA
}

object Test {
  def main(argv: Array[String]) = {
    val barLong: Bar { type FA = Long } = new Bar(new Foo { type FA = Long })
  }
}
