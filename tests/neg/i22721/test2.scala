package example

private def foobar: Int = 0 // error
object test2 { def x = foobar }
