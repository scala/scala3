package scala.test

import annotation.since

@since("") // error
val x = 1

@since("1.2.3.4") // error
val y = "abc"

@since("xyz") // error
class Foo

@since("-3") // error
trait Bar

@since("3.0.2") // error
type Baz = Int

@since("3.0 ") // error
given String = ""