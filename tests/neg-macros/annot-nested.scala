import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

class Foo:
  @experimental
  class void extends MacroAnnotation: // error
    def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = List(tree)

  object Bar:
    @experimental
    class void extends MacroAnnotation: // error
      def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = List(tree)

class Foo2:
  @experimental
  trait void extends MacroAnnotation // error

  object Bar:
    @experimental
    trait void extends MacroAnnotation // error

def test: Unit =
  @experimental
  class void extends MacroAnnotation: // error
    def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = List(tree)

  trait void2 extends MacroAnnotation // error

  new MacroAnnotation {} // error

  ()

val test2: Unit =
  @experimental
  class void extends MacroAnnotation: // error
    def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = List(tree)

  trait void2 extends MacroAnnotation // error

  new MacroAnnotation {} // error

  ()
