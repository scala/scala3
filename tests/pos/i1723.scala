class A {
  private val x: List[Int] = List(1)
  def foo = x.head // foo inferred type is this.x.scala$collection$immutable$List$$A
}

class B extends A {
  foo
}
