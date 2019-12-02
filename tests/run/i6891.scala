object Test {
  trait Foo { type X }
  class Bar extends Foo { type X = Int }
  type F = [A <: Foo] => (f: A) => f.X => f.X

  def main(args: Array[String]): Unit = {
    val g: F = [A <: Foo] => (f: A) => (x: f.X) => x
    println(g(new Bar)(1))
  }
}
