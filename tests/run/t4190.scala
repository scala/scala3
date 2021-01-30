import collection.mutable.*

object  Test extends App {
  val x: ArrayBuffer[String] = ArrayBuffer("a", "b", "c")
  x.view map (_ + "0") foreach println
}
