object Hello extends App:
  println("Hello, world!")
  println("Goodbye")

object WithArgs extends App:
  if args.nonEmpty then println(args(0))

object Config extends App:
  val name = "app"
  def greet() = println(name)
  greet()
  println("done")

class Api:
  def hello() = println("hi")

object Exporter extends App:
  val api = new Api
  export api.*
  println("run")

// an inline def appends a synthetic accessor to the typed body; the source layout
// split must ignore it (otherwise the trailing statement looks interleaved)
object Logger extends App:
  private def secret = 42
  inline def reveal = secret
  println(reveal)
