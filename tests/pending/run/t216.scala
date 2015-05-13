object Test extends dotty.runtime.LegacyApp {
  object m {
    val f = { x: Unit => () }
    Console.println("OK")
  }
  m;
}
