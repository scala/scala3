package foo

package object bar {
  type IOException = Object
  type Bippy[T] = List[T]
}

package object baz {
  type Bippy[+T] = List[T]
}

package baz {
  import java.io.*
  import foo.bar.*

  object Test {
    def f = new IOException // genuinely different
  }
}

package baz2 {
  import bar.*
  import baz.*

  object Test2 {
    def f: Bippy[Int] = ???
  }
}
