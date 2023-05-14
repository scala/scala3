object A {
  inline def isEven(inline x: Int): Boolean = {
    inline if x % 2 == 0 then
      true
    else
      !B.isOdd(x)
  }
}
