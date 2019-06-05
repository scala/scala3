import annotation.alpha
class D extends B_1 {
  @alpha("bar") def foo(): Int = 3
}
class E extends B_1 {
  @alpha("baz") override def bar(): Int = 3 // error: cannot have an @alpha annotation since external names would be different
}