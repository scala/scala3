class A with
  type T = Int // can also be class T
class B(x: A, y: A) with
  export x._
  export y._  // error: duplicate
class C(x: A) with
  type T = String
  export x._  // error: duplicate
class D(x: A) with
  export x._  // error: duplicate
  type T = String


