object Test extends dotty.runtime.LegacyApp {
  val a = 1
  val s = f"$a%s%n$a%s"
  println(s)
}
