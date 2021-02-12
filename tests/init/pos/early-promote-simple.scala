class P() {
    val a = 1
    List(this)
}

class Outer {
    class Inner {
        val b = a
    }
    val a = 5
    val b = new Inner()
    List(new Inner())
    List(b)
}