object Unit2Char extends App {
  val y: Unit = ()
  // Will throw java.lang.ClassCastException: scala.runtime.BoxedUnit cannot be cast to java.lang.Character
  // Should emit warning at compile time
  val x: Char = y.asInstanceOf[Char]
  println("cast was successful: " + x) // will not happen
}
