class A {
  private val x = List(1,2)

  val elem = x.head
}

class B extends A {
  val a: Int = elem
    // Without `checkNoPrivateLeaks`, we get:
    // found:    B.this.x.scala$collection$immutable$List$$A(B.this.elem)
    // required: Int
}
