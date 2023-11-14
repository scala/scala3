//> using options -Yno-experimental

import scala.annotation.experimental

class Foo:
  def foo: Int = experimentalDef // error

class Bar:
  def bar: Int = experimentalDef // error
object Bar:
  def bar: Int = experimentalDef // error

object Baz:
  def bar: Int = experimentalDef // error

def toplevelMethod: Int = experimentalDef // error

@experimental def experimentalDef: Int = 1
