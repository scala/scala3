object Test extends dotty.runtime.LegacyApp {
  def foo() = {
    while (true) {
      try {
        Console.println("foo")
      } catch {
        case ex: Exception =>
          Console.println("bar")
      }
    }
  }
}
