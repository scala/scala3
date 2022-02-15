class Foo:
  def repeat(@pos n: Int, a: Int): List[Int] =
    if (n == 0) Nil
    else a :: repeat(n - 1, a)

@main def Test =
  assert((new Foo).repeat(3, 1) == List(1, 1, 1))
