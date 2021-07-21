class A {
    def foo(b: B): A = {
        val temp = b
        temp.bar(this) // error
    }
    val x = foo(new B)
}

class B {
    def bar(a: A): A = a.x
}