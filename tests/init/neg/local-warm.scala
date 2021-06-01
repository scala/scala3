abstract class A {
    def m() = 10
    def m1(): B = new B
    def m2(): Int = m1().m()
    class B extends A {
        def x = 10
    }
}

class C extends A {
    def g() = {
        val t = m1() // error
        t.x
    }
    val x = g()
}
