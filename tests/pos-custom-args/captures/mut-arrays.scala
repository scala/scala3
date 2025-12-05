

def Test =
  val a1 = new Array[Int](3)
  val a2 = Array.fill(3)(0)
  val a3 = a1 ++ a2

  def f = () => a1(0) = 2
  def g = () => a1(0)
