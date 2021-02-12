package foo

package object bar {
  type IOException = java.io.IOException
}

package baz {
  import java.io.*
  import foo.bar.*

  object Test {
    def f = new IOException
  }
}
