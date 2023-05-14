object Test:
  opaque type Tup = (Int, String)

  def test(tup: Tup) =
    val (n, s) = tup
    assert(n == 1 && s == "")

  def main(args: Array[String]): Unit = test((1, ""))
