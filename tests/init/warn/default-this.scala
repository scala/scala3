class A {
    var x: Int = 10
    def compare(c: Int = 5, a: A = this): Boolean = if (c == a.x) true else false
}

class B extends A {
    def updateThenCompare(c: Int): Boolean = {
        x = c
        compare() // warn
    }
    val result = updateThenCompare(5)
}
