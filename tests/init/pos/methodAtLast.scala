class Foo(x: Int) {
    var y: Int = x
    case class Bar(z: Int) extends Foo(z)
    def updateY(n: Int): Unit = {
        if (y < 20) {
            val b = new Bar(x + n)
            y = b.z
        }
    }
    updateY(5)
}
