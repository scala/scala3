case class A(b: B) {
    val x1 = b.x
    val x = B(this)   // warn
    val y = x.a
}

case class B(a: A) {
    val x1 = a.x
    val x = A(this)   // warn
    val h = x.b
}
