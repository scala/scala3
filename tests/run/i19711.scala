class Foo(val s: Any):
  def this(s: String) =
    this(0)
class Bar(s: String) extends Foo(s):
  def foo = s

class Foo2(val s: Any)
class Bar2(s: String) extends Foo2(s):
  def foo = s

case class Config(_config: String)

abstract class Foo3(val config: Config) {
  def this(config: String) = {
    this(Config(config))
  }
}

class Bar3(config: String) extends Foo3(config) {
  def foo(): Unit = {
    config.getClass()
  }
}


@main def Test =
  Bar("").foo
  Bar2("").foo
  Bar3("").foo()
