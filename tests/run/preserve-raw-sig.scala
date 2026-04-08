trait MapFactory[+CC[_, _]]

trait Map[K, +V]:
  def mapFactory: MapFactory[Map] = ???

object Test:
  def main(args: Array[String]): Unit =
    classOf[Map[Int, String]].getMethods.filter(_.getName == "mapFactory").foreach(m =>
      println(m)
      println(m.toGenericString)
    )
