package test

object O {

  def byname(xs: => Int*) = xs.sum
  def byval(xs: Int*) = xs.sum

  def a = 1
  def b = 2

  byval(a, b)
  byname(a, b)

}

