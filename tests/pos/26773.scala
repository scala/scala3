trait HttpHeader
class Accept extends HttpHeader

def parseLine(s: String): (Int, HttpHeader) = (0, new Accept)

@main def run(): Unit =
  val (_, accept: Accept) = parseLine("Accept: text/plain").runtimeChecked
  println(accept)
