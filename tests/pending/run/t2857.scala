object Test extends dotty.runtime.LegacyApp {
  import collection.mutable._
  val m = new HashMap[Int, Set[String]] with MultiMap[Int, String]
  m.addBinding(6, "Foo")
  m.removeBinding(6, "Foo")
  println(m.contains(6))
}


