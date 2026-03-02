// https://github.com/scala/scala3/issues/12032
class Foo(val strings: Seq[String]) extends FooLowPriority

trait FooLowPriority { self: Foo =>
  @scala.annotation.targetName("appendFromProducers")
  def append(v: String): Foo = new Foo(v +: self.strings)
}

trait FooBar { self: Foo =>
  @scala.annotation.targetName("appendFromProducers")
  final override def append(v: String): Foo = new Foo(self.strings :+ v)
}

object Foo {
  type Bar = Foo with FooBar

  def bar(vs: String*): Bar = new Foo(vs) with FooBar
}

@main def Test() =
  Foo.bar("foo")
    .append("bar")
    .strings
    .foreach(println)
