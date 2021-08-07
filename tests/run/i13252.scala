object Test:

  class Star

  trait Foo:
    @annotation.targetName("star")
    val * : Star = new Star

  object Bar extends Foo

  def main(sa: Array[String]): Unit =
    Bar.*