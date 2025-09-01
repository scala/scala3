//> using options -experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*

class ImplicitValue

object ImplicitValue:
  inline given ImplicitValue =
    ${ makeImplicitValue }

  def makeImplicitValue(using Quotes) =
    import quotes.reflect.*
    '{ ImplicitValue() }
end ImplicitValue

@experimental
class Test extends MacroAnnotation:
  def transform(using Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]) =
    import quotes.reflect.*
    Implicits.search(TypeRepr.of[ImplicitValue])
    List(definition)
