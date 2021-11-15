import scala.quoted.Quotes
import scala.quoted.quotes
import scala.tasty.inspector as ins

// Test for https://github.com/lampepfl/dotty/issues/13919
class MyInspector extends ins.Inspector:
  def inspect(using Quotes)(tastys: List[ins.Tasty[quotes.type]]): Unit =
    import quotes.reflect._
    TypeRepr.of[Int]
    TypeRepr.of[String]


@main def main(args: String*): Unit =
  ins.TastyInspector.inspectTastyFilesInJar(args.head)(new MyInspector)
