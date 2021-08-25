case class A(x: Int) {
    def foo(y: Int): B = if (y > 10) then B(bar(y - 1), foo(y - 1).getN) else B(bar(y), 10)
    def bar(y: Int): A = if (y > 10) then A(y - 1) else this
    class B(a: A, b: Int) {
        def getN: Int = a.n // error
        def getB: Int = b
    }
    println(foo(x).getB)
    val n: Int = 10
}