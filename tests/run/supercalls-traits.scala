trait A {
 def foo = 1
}

trait B {
 def foo = 2
}

class C extends A with B {
 override def foo = super[A].foo + super[B].foo
}

class Base[A](exp: => Option[A])

object Empty extends Base[Nothing](None)

object Test {
  def main(args: Array[String]): Unit = {
    assert(new C().foo == 3)
    Empty
  }
}
