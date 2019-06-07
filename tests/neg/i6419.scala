trait A {
  @scala.annotation.compileTimeOnly("oops") def f: Int
}

class B extends A {
  def f = 0
}

object App {
  (new B).f
  (new B: A).f // error
}
