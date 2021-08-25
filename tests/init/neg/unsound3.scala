class B(c: C) {
    def getC() = c
}

class C {
    var x = 10
    def foo(): B = {
        x += 1
        val newB = new B(this)
        if (x < 12) then foo().getC().b else newB // error
    }
    val b = foo()
}