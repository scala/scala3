class A() {
    var x = 5
    def decreaseX(): Unit = {
        val self = this
        self.x -= 1
    }
    def decreaseXToZero(): Unit = if x > 0 then decreaseX()
    decreaseXToZero()
    val y = x
}