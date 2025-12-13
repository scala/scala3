object ListOperations {
  def main(args: Array[String]): Unit = {
    val nums = List(1, 2, 3, 4, 5)

    println(s"Original: ${nums.mkString(", ")}")
    println(s"Doubled: ${nums.map(_ * 2).mkString(", ")}")
    println(s"Evens: ${nums.filter(_ % 2 == 0).mkString(", ")}")
    println(s"Sum: ${nums.foldLeft(0)(_ + _)}")
    println(s"Product: ${nums.foldLeft(1)(_ * _)}")

    // Higher-order functions
    val squares = nums.map(x => x * x)
    println(s"Squares: ${squares.mkString(", ")}")

    // Chaining
    val result = nums
      .filter(_ > 2)
      .map(_ * 10)
      .take(2)
    println(s"Chained: ${result.mkString(", ")}")
  }
}

