object Test {
  type NonNeg = {v: Int with v >= 0}

  def safeAdd(x: NonNeg, y: NonNeg): NonNeg =
    if x + y < 0 then 2147483647 else x + y

  def sumNat(n: Int): NonNeg =
    if n <= 0 then
      0
    else
      safeAdd(sumNat(n - 1), n)
}
