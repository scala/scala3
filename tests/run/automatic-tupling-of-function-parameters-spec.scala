
object Test {

  def main(args: Array[String]): Unit = {
    val xs: List[(Int, Int)] = (1, 2) :: (3, 4) :: (5, 6) :: Nil

    println(xs.map {
      case (x, y) => x + y
    })

    println(xs.map {
      (x, y) => x + y
    })

    println(xs.map(_ + _))
  }

}
