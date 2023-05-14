object B {
  inline def isOdd(inline x: Int): Boolean = {
    inline if x % 2 != 0 then
      true
    else
      !A.isEven(x)
  }
}
