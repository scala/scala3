package tests
package infixTypes

import annotation.showAsInfix

@showAsInfix
infix trait SomeTrait[A, B]

infix trait SomeTrait2[A, B]

def someTrait1[C, D]: C SomeTrait D
  = ???

def someTrait2[E, F]: SomeTrait[E, F] //expected: def someTrait2[E, F]: E SomeTrait F
  = ???

def someTrait3[G, H]: G SomeTrait2 H //expected: def someTrait3[G, H]: SomeTrait2[G, H]
  = ???

trait +++[A, B]

trait ++*[A, B]

trait ++:[A, B]

trait +*:[A, B]

trait ***[A, B]

trait **:[A, B]

def foo[A, B, C, D]: (A SomeTrait B) +++ (C SomeTrait2 D) //expected: def foo[A, B, C, D]: (A SomeTrait B) +++ SomeTrait2[C, D]
  = ???

// left-associative, same precedence

def a0[X, Y, Z]: X +++ Y +++ Z
  = a1

def a1[X, Y, Z]: (X +++ Y) +++ Z //expected: def a1[X, Y, Z]: X +++ Y +++ Z
  = a0

def a2[X, Y, Z]: X +++ (Y +++ Z)
  = ???

def a0x[X, Y, Z]: X +++ Y ++* Z //expected: def a0x[X, Y, Z]: (X +++ Y) ++* Z
  = a1x

def a1x[X, Y, Z]: (X +++ Y) ++* Z
  = a0x

def a2x[X, Y, Z]: X +++ (Y ++* Z)
  = ???

// right-associative, same precedence

def a3[X, Y, Z]: X ++: Y ++: Z
  = a5

def a4[X, Y, Z]: (X ++: Y) ++: Z
  = ???

def a5[X, Y, Z]: X ++: (Y ++: Z) //expected: def a5[X, Y, Z]: X ++: Y ++: Z
  = a3

def a3x[X, Y, Z]: X ++: Y +*: Z  //expected: def a3x[X, Y, Z]: X ++: (Y +*: Z)
  = a5x

def a4x[X, Y, Z]: (X ++: Y) +*: Z
  = ???

def a5x[X, Y, Z]: X ++: (Y +*: Z)
  = a3x

// left and right associative, same precedence

def a6[X, Y, Z]: (X +++ Y) ++: Z
  = ???

def a7[X, Y, Z]: X +++ (Y ++: Z)
  = ???

// left-associative, mixed precedence

def b0[X, Y, Z]: X +++ Y *** Z //expected: def b0[X, Y, Z]: X +++ (Y *** Z)
  = ???

def b1[X, Y, Z]: (X +++ Y) *** Z
  = ???

def b2[X, Y, Z]: X +++ (Y *** Z)
  = ???

def b3[X, Y, Z]: X *** Y +++ Z //expected: def b3[X, Y, Z]: (X *** Y) +++ Z
  = ???

def b4[X, Y, Z]: (X *** Y) +++ Z
  = ???

def b5[X, Y, Z]: X *** (Y +++ Z)
  = ???

// right-associative, mixed precedence

def c0[X, Y, Z]: X ++: Y **: Z //expected: def c0[X, Y, Z]: X ++: (Y **: Z)
  = ???

def c1[X, Y, Z]: (X ++: Y) **: Z
  = ???

def c2[X, Y, Z]: X ++: (Y **: Z)
  = ???

def c3[X, Y, Z]: X **: Y ++: Z //expected: def c3[X, Y, Z]: (X **: Y) ++: Z
  = ???

def c4[X, Y, Z]: (X **: Y) ++: Z
  = ???

def c5[X, Y, Z]: X **: (Y ++: Z)
  = ???

// left and right associative, mixed precedence

def d0[X, Y, Z]: X +++ Y **: Z //expected: def d0[X, Y, Z]: X +++ (Y **: Z)
  = ???

def d1[X, Y, Z]: (X +++ Y) **: Z
  = ???

def d2[X, Y, Z]: X +++ (Y **: Z)
  = ???

def d3[X, Y, Z]: X *** Y ++: Z //expected: def d3[X, Y, Z]: (X *** Y) ++: Z
  = ???

def d4[X, Y, Z]: (X *** Y) ++: Z
  = ???

def d5[X, Y, Z]: X *** (Y ++: Z)
  = ???
