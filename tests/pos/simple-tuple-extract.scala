
class Test:
  def f1: (Int, Int, Int) = (1, 2, 3)
  def f2: (x: Int, y: Int) = (3, 4)

  def test1 =
    val (a, b, c) = f1
    // Desugared to:
    // val $2$: (Int, Int, Int) =
    //   this.f1:(Int, Int, Int) @unchecked match
    //     {
    //       case $1$ @ Tuple3.unapply[Int, Int, Int](_, _, _) =>
    //         $1$:(Int, Int, Int)
    //     }
    // val a: Int = $2$._1
    // val b: Int = $2$._2
    // val c: Int = $2$._3
    a + b + c

  def test2 =
    val (_, d, e) = f1
    e + e

  def test3 =
    val (_, f, _) = f1
    f + f

  def test4 =
    val (x, y) = f2
    x + y

  def test5 =
    val (_, a) = f2
    a + a
