object effects extends App {

  def f1(implicit erased x: CanThrow[NullPointerException]) = 3
  def f2(x: Int)(implicit erased y: CanThrow[NullPointerException]) = f1(y)

  assert(f1 == 3)
  assert(f2(3) == 3)

}
