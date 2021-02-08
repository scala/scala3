class Foo[A]

object Test {
  def foo[T](x: Foo[T]) = x

  foo((new Foo[Int]: Foo[_]))
}

import java.nio.file.*
import java.util.stream.Collectors

object Foo {
  Files.walk(Paths.get("")).collect(Collectors.toList[Path])
}


