class A(val a: A)
object B:
    val a: A = loop().a
    def loop(): A = new A(loop())