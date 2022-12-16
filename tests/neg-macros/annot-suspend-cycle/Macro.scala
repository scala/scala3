import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class cycle extends MacroAnnotation {
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    new Foo
    List(tree)
}
