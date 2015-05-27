trait A {
 def foo = 1
}

trait B {
 def foo = 2
}

class C extends A with B {
 override def foo = super[A].foo + super[B].foo
}

object Test {
 def main(args: Array[String]) = assert(new C().foo == 3)
}
