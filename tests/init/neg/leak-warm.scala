object leakWarm {
    abstract class A(tag: Int) {
        class B(x: Int) {
            val y = x
        }
        def m(): B
    }

    class C(tag1: Int, tag2: Int) extends A(tag1) {
        def m() = new B(5)
    }

    class D(tag1: Int, tag2: Int) extends A(tag1 + tag2) {
        def m() = new B(tag1)
    }
    val c = new C(1, 2)
    val d = new D(3, 4)
    val l: List[A] = List(c, d) // error
    val l2 = l.map(_.m())
}
