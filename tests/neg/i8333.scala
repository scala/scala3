class A:
  type T = Int // can also be class T
class B(x: A, y: A):
  export x._
  export y._  // error: duplicate
class C(x: A):
  type T = String
  export x._  // error: duplicate
class D(x: A):
  export x._  // error: duplicate
  type T = String


