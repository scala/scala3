object Test {
   def foo: 1 | 2 = 1 // error // error
   def bar: 3 | 4 = foo // error // error
   def foo: 1 | 2 = 1 // error // error
   def bar: 1 = foo
}
