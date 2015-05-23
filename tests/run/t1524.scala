object Test extends dotty.runtime.LegacyApp {

  val buf = new scala.collection.mutable.ArrayBuffer[String](0)
  buf += "initial"
  buf += "second"
  println(buf.head)
}
