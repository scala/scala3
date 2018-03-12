object Test {
   def a: 1 | 2 = 1 // error // error
   def b: 3 | 4 = a // error // error
   def c: 1 | 2 = 1 // error // error
   def d: 1 = a
}
