package foo

import scala.annotation.inlineAccessible

@inlineAccessible type A // error
@inlineAccessible object B // error // TODO support?
@inlineAccessible class C // error
