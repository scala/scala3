object Test:
  def main(args: Array[String]): Unit =
    val list = List(1)
    for
      x <- list
      y <- list
      z = x + y
    yield x
    for
      x <- list
      if x == 1
    yield x
    for x <- list yield x
    for x <- list do println(x)