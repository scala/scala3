transparent def foo = 1  // error
transparent inline def bar = 2 // ok
transparent inline val x = 2 // error
transparent class c // error
transparent object y // error
transparent trait t // ok
transparent type T = c  // error
transparent given c() // error

