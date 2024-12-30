//> using options -preview

import scala.annotation.preview

class Foo:
  def foo: Int = previewDef

class Bar:
  def bar: Int = previewDef
object Bar:
  def bar: Int = previewDef

object Baz:
  def bar: Int = previewDef

def toplevelMethod: Int = previewDef

@preview def previewDef: Int = 1
