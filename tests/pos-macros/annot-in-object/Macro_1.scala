//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

object Foo:
  @experimental
  class void extends MacroAnnotation:
    def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = List(tree)

  object Bar:
    @experimental
    class void extends MacroAnnotation:
      def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = List(tree)
