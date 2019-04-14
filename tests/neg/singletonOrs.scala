object Test {
   def a: 1 | 2 = 1
   def b: 3 | 4 = a // error
   def c: 1 | 2 = 1
   def d: 1 = a // error
}
