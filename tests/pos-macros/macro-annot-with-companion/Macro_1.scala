import scala.annotation.MacroAnnotation

import scala.quoted.*

class transform extends MacroAnnotation:
  override def transform(using Quotes)(
    tree: quotes.reflect.Definition,
    companion: Option[quotes.reflect.Definition]
  ) : List[quotes.reflect.Definition] = {
    import quotes.reflect.*
    List(tree)
  }
