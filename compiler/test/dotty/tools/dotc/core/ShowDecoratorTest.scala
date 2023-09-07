package dotty.tools
package dotc
package core

import Contexts.*, Decorators.*, Denotations.*, SymDenotations.*, Symbols.*, Types.*
import printing.Formatting.Show

import org.junit.Test
import org.junit.Assert.*

class ShowDecoratorTest extends DottyTest:
  import ShowDecoratorTest.*

  @Test def t1 = assertEquals("... (cannot display due to FooException boom) ...", Foo().tryToShow)
end ShowDecoratorTest

object ShowDecoratorTest:
  import printing.*, Texts.*
  class FooException extends Exception("boom")
  case class Foo() extends Showable:
    def toText(printer: Printer): Text = throw new FooException
