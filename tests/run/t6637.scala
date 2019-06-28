object Test extends App {
  try {
    List().head
  } catch {
    case _ :java.util.NoSuchElementException => println("ok")
  }
}
