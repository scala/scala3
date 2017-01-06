
object Test {
  def main(args: Array[String]): Unit = {
    new Foo
  }
}

final class Foo extends Bar[Int]

trait Bar[T] {
  val value: Int = 42
}
