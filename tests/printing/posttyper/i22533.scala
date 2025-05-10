package scala

import scala.language.`2.13`

trait A extends Any:
    override def equals(x: Any): Boolean = ???
    override def hashCode(): Int = ???

class Foo(u: Int) extends AnyVal, A