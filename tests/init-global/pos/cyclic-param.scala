class B {}

class C(c: B) extends B

object O:
    def f(param: B): Int = f(new C(param))
    val a = f(new B)