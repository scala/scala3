//> using options -Wunused:all
object Foo {
  val dummy = 42
  def f(): Unit = Option(1).map((x: Int) => dummy) // warn
  def g(): Unit = Option(1).map((x: Int) => ???) // warn
  def main(args: Array[String]): Unit = {}
}
