def f = {
  val smap: Map[String, String] = ???
  val ss = smap.map { case (n, v) => (n, n + v) }
}