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

  @annotation.tailrec
  def isEvenApply(n: Int): Boolean =
    // Return inside an `Apply.fun`
    (
      if n != 0 && n != 1 then return isEvenApply(n - 2)
      else if n == 1 then return false
      else (x: Boolean) => x
    )(true)

  @annotation.tailrec
  def isEvenWhile(n: Int): Boolean =
    // Return inside a `WhileDo.cond`
    while(
      if n != 0 && n != 1 then return isEvenWhile(n - 2)
      else if n == 1 then return false
      else true
    ) {}
    true

  @annotation.tailrec
  def isEvenReturn(n: Int): Boolean =
    // Return inside a `Return`
    return
      if n != 0 && n != 1 then return isEvenReturn(n - 2)
      else if n == 1 then return false
      else true

  @annotation.tailrec
  def names(l: List[(String, String) | Null], acc: List[String] = Nil): List[String] =
    l match
      case Nil => acc.reverse
      case x :: xs =>
        if x == null then return names(xs, acc)

        val displayName = x._1 + " " + x._2
        names(xs, displayName :: acc)

  def nonTail(l: List[Int]): List[Int] =
    l match
      case Nil => Nil
      case x :: xs =>
        // The call to nonTail should *not* be eliminated
        (x + 1) :: nonTail(xs)


  def main(args: Array[String]): Unit =
    println(sum(3))
    println(isEven(5))
    println(isEvenApply(6))
    println(isEvenWhile(7))
    println(isEvenReturn(8))
    println(names(List(("Ada",  "Lovelace"), null, ("Alan", "Turing"))).mkString(", "))
    println(nonTail(List(8, 9)))
