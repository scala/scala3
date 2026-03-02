object Test:
  def main(args: Array[String]): Unit =
    val ! = "!"
    println(| + new <> + &(":") + !)
  private val | = "|"
  private class <> :
    override def toString: String = "<>"
  private def &(`:`: String): String = s"&(${`:`})"