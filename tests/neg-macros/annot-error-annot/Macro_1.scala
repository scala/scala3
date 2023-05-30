import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class error extends MacroAnnotation {
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    quotes.reflect.report.error("MACRO ERROR", tree.pos)
    List(tree)
}
