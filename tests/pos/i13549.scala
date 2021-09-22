@FunctionalInterface
trait Executable {
  def execute(): Unit
}

object Test {
  def assertThrows(executable: Executable, message: String): Unit = ???
  def assertThrows(executable: Executable, foo: Int): Unit = ???

  assertThrows(() => 3, "This is a message")
}
