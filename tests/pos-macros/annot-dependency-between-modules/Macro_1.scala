import scala.annotation.*
import scala.quoted.*

@experimental
class void extends MacroAnnotation:
  def transform(using Quotes)(
    definition: quotes.reflect.Definition,
    companion: Option[quotes.reflect.Definition]
  ) : List[quotes.reflect.Definition] =
    definition +: companion.toList
  end transform
