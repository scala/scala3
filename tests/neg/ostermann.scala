trait A { def foo(a: A) : Unit }

trait C extends A {
  override def foo(a: A & Any): Unit // error: method foo has a different signature than the overridden declaration
}
