object Main {
  def foo(x: Option[Int]): Int = x match {
    case Some(n) if n % 2 == 0 => n
    case None => 0
  }

  def main(args: Array[String]): Unit = println(foo(Some(1)))
}