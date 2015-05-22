object Test extends dotty.runtime.LegacyApp {
  println(identity('a').toInt)
  println('a'.toInt)
  println(identity('a').asInstanceOf[Int])
  println('a'.asInstanceOf[Int])
  println(identity(1).asInstanceOf[Int])
  println(1.asInstanceOf[Int])
}
