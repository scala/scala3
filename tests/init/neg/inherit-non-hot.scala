// This is a minimized test for the warning in Names.scala:174
object Foo {
    abstract class A {
        var b: B = null
        def toB: B =
            if b == null then b = new B(this) // error
            b
    }

    class B(a: A) {
        var aCopy: A = a
        def getBAgain: B = aCopy.toB
    }

    class C extends A {
        val bAgain = toB.getBAgain
    }

    val c = new C
    assert(c.b == c.bAgain)
}