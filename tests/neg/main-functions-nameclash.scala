object foo {
  @main def foo(x: Int) = () // error: class foo and object foo produce classes that overwrite one another
}
