import scala.collection.mutable.HashSet


object Test extends dotty.runtime.LegacyApp {
  val h = new HashSet[Int]
  h += 1
  println(s"remove 0 should be false, was ${h remove 0}")
  println(s"contains 1 should be true, was ${h contains 1}")
  println(s"remove 1 should be true, was ${h remove 1}")
  println(s"contains 1 should be false, was ${h contains 1}")
  println(s"remove 1 should be false, was ${h remove 1}")
  println(s"contains 1 should be false, was ${h contains 1}")
 }
