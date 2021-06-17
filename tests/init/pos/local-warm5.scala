object leakWarm5 {
    case class A(x: Int) {
        def double(): A = {
            class C {
                def double(): A = if x < 10 then A(x * 2).double() else A.this
            }
            val c = new C
            c.double()
        }
    }
    val a = A(2).double()
}
