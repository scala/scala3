object Test:

  @annotation.tailrec
  def sum(n: Int, acc: Int = 0): Int =
    if n != 0 then return sum(n - 1, acc + n)
    acc

  @annotation.tailrec
  def isEven(n: Int): Boolean =
    if n != 0 && n != 1 then return isEven(n - 2)
    if n == 1 then return false
    true

  def main(args: Array[String]): Unit =
    println(sum(3))
    println(isEven(5))
