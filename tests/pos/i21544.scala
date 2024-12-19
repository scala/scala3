class Test():
  def m1(xs: List[Boolean]) = for (x: Any) <- xs yield x
  def m2(xs: List[Boolean]) = for (x: AnyVal) <- xs yield x
  def m3(xs: List[Boolean]) = for (x: Matchable) <- xs yield x

  def v1(xs: List[AnyVal])    = for (x: Any) <- xs yield x
  def v2(xs: List[AnyVal])    = for (x: AnyVal) <- xs yield x
  def v3(xs: List[AnyVal])    = for (x: Matchable) <- xs yield x

  def t1(xs: List[Matchable]) = for (x: Any) <- xs yield x
  def t2(xs: List[Matchable]) = for (x: Matchable) <- xs yield x

  def a1(xs: List[Any])       = for (x: Any) <- xs yield x
