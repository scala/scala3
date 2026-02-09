import language.experimental.captureChecking

trait Test {
    def foo(x: Test): Test =
        Test.bar
        ???
}

object Test {
    val _bar: Any => Any = identity
    def bar[T] = _bar.asInstanceOf[T => T] // error
}
