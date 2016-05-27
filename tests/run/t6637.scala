object Test extends dotty.runtime.LegacyApp {
  try {
    List().head
  } catch {
    case _ :java.util.NoSuchElementException => println("ok")
  }
}
