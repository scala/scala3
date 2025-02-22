//> using options -experimental

import scala.annotation.publicInBinary

class Foo:
    @publicInBinary private def this(x: Int) = this()
    inline def proxy: Foo = new Foo(0)

@main def Test = 
    val x = (new Foo).proxy
