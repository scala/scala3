object effects extends App {

  def f1(implicit x: CanThrow[NullPointerException]) = 3
  def f2(x: Int)(implicit y: CanThrow[NullPointerException]) = x

  assert(f1 == 3)
  assert(f2(3) == 3)

}
