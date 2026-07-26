trait T:
  def f(): String

trait T2:
  val t: T

object Foo extends T2:
  object t extends T:
    def f(): String = "hello"

object Test:
  def t2(): T2 = Foo
  def main(args: Array[String]): Unit =
    println(t2().t.f())
