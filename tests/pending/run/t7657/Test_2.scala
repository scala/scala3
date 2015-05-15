object Test extends dotty.runtime.LegacyApp {
  val c = new C()
  println(c.t())
  println((c: T).t())
  println((c: A).t())
}
