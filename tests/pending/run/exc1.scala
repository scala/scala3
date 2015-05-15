object Test extends dotty.runtime.LegacyApp {
  def foo(): Unit = {
    while (true) {
      try {
      } catch {
        case ex: Exception =>
      }
    }
  }
}
