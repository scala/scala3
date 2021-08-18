import scala.quoted.Quotes
import scala.quoted.quotes
import scala.tasty.inspector as ins

class MyInspector extends ins.Inspector:
  def inspect(using Quotes)(tastys: List[ins.Tasty[quotes.type]]): Unit =
    val sources = tastys.map(_.ast.pos.sourceFile.toString).toSet
    val expectedSources = Set("lib/src/main/scala/toplevel.scala", "lib/src/main/scala/inpackage.scala")
    assert(sources == expectedSources, s"expected $expectedSources tasty files but get: $sources")

@main def main(args: String*): Unit =
  ins.TastyInspector.inspectTastyFilesInJar(args.head)(new MyInspector)
