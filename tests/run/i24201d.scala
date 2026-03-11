class C(i: Int = 42, j: => Int = 27):
  def value: Int = j

object X extends C(j = X.k):
  def k = 5

@main def Test =
  println(X.value)
