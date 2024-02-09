import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*

@experimental
class annotation extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition) = List(tree)
