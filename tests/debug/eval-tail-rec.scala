object Test:
  def main(args: Array[String]): Unit =
    println(f(80))

  @scala.annotation.tailrec
  def f(x: Int): Int =
    if x <= 42 then x
    else f(x/2)

