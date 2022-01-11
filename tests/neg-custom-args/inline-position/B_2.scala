object B {
  erased val y: Array[Char]

  A.f(new String(y)) // error: value y is declared as erased but is in fact used
}