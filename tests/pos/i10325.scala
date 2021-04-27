object Test {
  def nullToNone[K, V](tuple: (K, V)): (K, Option[V]) = {
    val (k, v) = tuple
    (k, Option(v))
  }

  def test: Unit = {
    val scalaMap: Map[String, String] = Map()

    val a = scalaMap.map(nullToNone)
    val a1: Map[String, Option[String]] = a

    val b = scalaMap.map(nullToNone(_))
    val b1: Map[String, Option[String]] = b

    val c = scalaMap.map(x => nullToNone(x))
    val c1: Map[String, Option[String]] = c
  }
}
