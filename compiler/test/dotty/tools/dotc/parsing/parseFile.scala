package dotty.tools.dotc.parsing

object parseFile extends ParserTest {

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) println("usage: scala test.parseFile file1.scala ... fileN.scala")
    for (arg <- args) {
      val tree = parse(arg)
      println("parsed: " + arg)
      println(tree.show)
    }
  }
}
