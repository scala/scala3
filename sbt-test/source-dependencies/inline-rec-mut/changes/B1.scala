object B {
  inline def isOdd(inline x: Int): Boolean = {
    inline if x % 2 == 0 then
      val cached = A.isEven(x)
      !(cached || cached)
    else
      true
  }
}
