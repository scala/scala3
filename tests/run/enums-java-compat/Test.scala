object Test {
  def main(args: Array[String]): Unit = {
    val t1 = B.EARTH
    val t2 = B.JUPITER

    println("name:     " + t1.name)
    println("ordinal:  " + t1.ordinal)
    println("toString: " + t1.toString)

    val values: Array[A] = A.values
    println("Values class: " + values.getClass)
    values.foreach(v => println(v.name + " : " + v.ordinal))
    println("By-name value: " + A.valueOf("MONDAY"))
    try A.valueOf("stuff")
    catch { case e: IllegalArgumentException =>
      println("Correctly failed to retrieve illegal name, message: " + e.getMessage)
    }
  }
}