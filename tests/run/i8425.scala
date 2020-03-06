import java.io.{ OutputStream, PrintStream }

trait T {
  val text: String
  val stream = new PrintStream(new OutputStream {
    def write(b: Int) = Console.println(s"text: $b")
  }) {
    override def println(x: Any) = ???
  }
}

@main def Test =
  val t = new T { val text = "hello" }
  t.stream.write(22)
  t.stream.println('A')
