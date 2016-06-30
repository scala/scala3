object Test {
  case class Car(`type`: String, year: Int)

  def main(args: Array[String]): Unit = {
    println(Car("Mustang", 1977).toMap)
  }
}
