import collection.mutable._

object  Test extends dotty.runtime.LegacyApp {
  val x: ArrayBuffer[String] = ArrayBuffer("a", "b", "c")
  x.view map (_ + "0") foreach println
}
