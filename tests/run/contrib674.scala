// causes VerifyError with scala-2.5.1

object Test extends dotty.runtime.LegacyApp {
  def bad(): Unit = {
    try {
      1
    } catch {
      case e: Throwable =>
    } finally {
      try {
      } catch {
        case e: Throwable =>
      }
    }
    1
  }

  bad()
}
