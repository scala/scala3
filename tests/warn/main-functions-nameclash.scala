

object foo {
  @main def foo(x: Int) = () // warn: class foo and object foo produce classes that overwrite one another
}
