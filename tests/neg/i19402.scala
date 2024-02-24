object Test: 

  class Bar(foo: Foo)
  
  class Foo

  given (Foo) = ???
  given (using a: Int): Int = ???
  given [T](using a: T): T = ???
  given bar(foo: Foo): Bar  = Bar(foo) // error: using is expected
  