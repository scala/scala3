case class A(b: B) {
    val x1 = b.x      // error
    val x = B(this)
    val y = x.a
}

case class B(a: A) {
    val x1 = a.x      // error
    val x = A(this)
    val h = x.b
}
