
class Test:
  def f1: (Int, String, AnyRef) = (1, "2", "3")
  def f2: (x: Int, y: String) = (0, "y")

  def test1 =
    val (a, b, c) = f1
    // Desugared to:
    // val $2$: (Int, String, AnyRef) =
    //   this.f1:(Int, String, AnyRef) @unchecked match
    //     {
    //       case $1$ @ Tuple3.unapply[Int, String, Object](_, _, _) =>
    //         $1$:(Int, String, AnyRef)
    //     }
    // val a: Int = $2$._1
    // val b: String = $2$._2
    // val c: AnyRef = $2$._3
    a + b.length() + c.toString.length()

    // This pattern will not be optimized:
    // val (a1, b1, c1: String) = f1

  def test2 =
    val (_, b, c) = f1
    b.length() + c.toString.length()

    val (a2, _, c2) = f1
    a2 + c2.toString.length()

    val (a3, _, _) = f1
    a3 + 1

  def test3 =
    val (_, b, _) = f1
    b.length() + 1

  def test4 =
    val (x, y) = f2
    x + y.length()

  def test5 =
    val (_, b) = f2
    b.length() + 1