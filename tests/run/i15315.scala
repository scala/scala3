class A:
  def f(x: Int = 1): Int = x

@main def Test() =
  (new A{}).f()
