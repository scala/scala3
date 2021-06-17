class A {
    var v = 5
    val x = {
        class B {
            def doubleAndReturnV(): Int = {
                v = v * 2
                v
            }
        }
        val b = new B
        b.doubleAndReturnV()
    }
    val y = v
}