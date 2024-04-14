//> using options -experimental -Yno-experimental

import scala.annotation.MacroAnnotation
import scala.quoted.*

class buggy extends MacroAnnotation:

  def transform(using Quotes)
               (definition: quotes.reflect.Definition,
                companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =

    import quotes.reflect.*
    companion match
      case Some(companion) =>
        List(definition, companion, companion)
      case None =>
        report.error("The goal of this test is to return the companion more than once to trigger a compilation error")
        List(definition)
  end transform