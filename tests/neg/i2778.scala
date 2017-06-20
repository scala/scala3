object Main {
  def printName(first: String, last: String): Unit = {
    println(first + " " + last)
  }

  def main(args: Array[String]): Unit = {
    printName("John", first = "Smith") // error: parameter is already instantiated
  }
}