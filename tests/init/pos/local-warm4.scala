object localWarm {
    abstract class Foo(x: Int) {
        def increment(): Unit
    }

    class A(x: Int) extends Foo(x) {
        var y = x
        override def increment(): Unit = y = y + 1
        increment()
        val b = new B(y)
    }

    class B(x: Int) extends A(x) {
        var a: A = this
        override def increment(): Unit = {
            def updateA(): Unit = {
                val newA = new A(y)
                a = newA // ok: newA can be promoted to hot
            }
            y = y + 1
            updateA()
        }
        if y < 10 then increment()
        val z = b.y
    }
    val a = new A(5)
}
