class Test():
  def m1(xs: List[Boolean]) = for (x: Any) <- xs yield x
  def m2(xs: List[Boolean]) = for (x: AnyVal) <- xs yield x
  def m3(xs: List[Boolean]) = for (x: Matchable) <- xs yield x
