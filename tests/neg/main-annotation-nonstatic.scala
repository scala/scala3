class myMain extends main

class A:
  @main def foo(bar: Int) = () // error
  @myMain def baz() = () // error