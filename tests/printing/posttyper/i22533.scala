//> using options -Ycompile-scala2-library

trait A extends Any:
    override def equals(x: Any): Boolean = ???
    override def hashCode(): Int = ???

class Foo(u: Int) extends AnyVal, A