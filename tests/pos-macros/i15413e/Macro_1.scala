package myMacro

import scala.quoted.*

class Macro:
  inline def foo = ${ Foo.impl }
  inline def bar = ${ Bar.impl }
  inline def baz = ${ pack.Foo.impl }

object Foo:
  private[myMacro] def impl(using Quotes) = '{}

object Bar:
  private[myMacro] def impl(using Quotes) = '{1}

package pack:
  object Foo:
    private[myMacro] def impl(using Quotes) = '{"abc"}
