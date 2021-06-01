class A {
    val x = 10
    class B(a: A) {
        val anotherX = A.this.x
    }
    val b = B(this)
    val xAgain = b.anotherX
}
