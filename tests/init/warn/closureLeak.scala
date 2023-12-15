class Outer {
    val x = 10
    class A(x: Int) {
        var y: Int = x
        def addX(o: Outer): Unit = {
            y = y + o.x
        }
    }

    val l: List[A] = List(new A(5), new A(10))
    l.foreach(a => a.addX(this)) // warn
    val p = 10
}
