trait A {
    def foo = 4
}
object B extends A {
    private[this] def foo = 0
}