//> using options -experimental

import scala.annotation.publicInBinary

class Foo:
    private def this(x: Int) = this()
    inline def proxy: Foo = new Foo(0) // error
