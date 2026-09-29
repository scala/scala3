import annotation._

@SerialVersionUID(42) class Foo[@specialized(Int) T] extends Serializable {
  def foo(t: T) = t
}

object Test extends App {
  def checkUID(cls: Class[_], expected: Long) = {
    val actual = java.io.ObjectStreamClass.lookup(cls).getSerialVersionUID
    assert(actual == expected, s"$actual != expected for ${cls}")
  }
  def check(x: AnyRef): Unit = {
    checkUID(x.getClass, 42)
  }

  check(new Foo[String])
  check(new Foo[Int])
}

