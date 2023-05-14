case class A(b: B) {
    val x1 = b.x
    val x = B(this)   // error
    val y = x.a
}

case class B(a: A) {
    val x1 = a.x
    val x = A(this)   // error
    val h = x.b
}
