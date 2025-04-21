
package dotty.tools.dotc.util

object chaining:

  extension [A](x: A)
    inline def tap(inline f: A => Unit): x.type = { f(x): Unit; x }
    inline def pipe[B](inline f: A => B): B = f(x)
