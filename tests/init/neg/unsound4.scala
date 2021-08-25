class A {
    def foo(x: Int): A = if (x < 5) then this else foo(x - 1).aAgain
    val aAgain = foo(5) // error
}