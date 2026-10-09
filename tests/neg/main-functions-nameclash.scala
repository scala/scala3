object foo { // error: class foo and object foo produce classes that overwrite one another
  @main def foo(x: Int) = ()
}
