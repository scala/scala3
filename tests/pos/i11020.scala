trait Bar
trait Baz
trait B[X]
extension (using Bar)(s: String)(using Baz) def foo = ???
extension (using Bar)(s: String) def bar(using Baz) = ???
extension [T: B](s: T) def baz [U: B](x: Int) = ???

