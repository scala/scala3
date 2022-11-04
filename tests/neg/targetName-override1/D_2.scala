import annotation.targetName
class D extends B_1 {
  @targetName("bar") def foo(): Int = 3
}
class E extends B_1 {
  @targetName("baz") override def bar(): Int = 3 // error: cannot have an @targetName annotation since external names would be different
}