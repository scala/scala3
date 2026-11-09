trait Factory[+CC[_, _]]

trait Map[K, +V]:
  def mapFactory: Factory[Map] = ???

class Other:
  def tupleFactory: Factory[Tuple2] = ???

object Test:
  def main(args: Array[String]): Unit =
    classOf[Map[Int, String]].getMethods.filter(_.getName == "mapFactory").foreach(m =>
      println(m)
      println(m.toGenericString)
    )
    println("---")
    classOf[Other].getMethods.filter(_.getName == "tupleFactory").foreach(m =>
      println(m)
      println(m.toGenericString)
    )

