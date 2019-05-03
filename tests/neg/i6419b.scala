trait A {
  inline def f: Int = scala.compiletime.error("oops")
}

class B extends A {
  override def f = 0
}

object App {
  (new B).f
  (new B: A).f // error
}
