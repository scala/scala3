import genericouterinnermember.*

object Test:
  def main(args: Array[String]): Unit =
    val outer = new Outer[String]
    println(Use_2.use(outer))
    println(Use_2.use2(outer))

