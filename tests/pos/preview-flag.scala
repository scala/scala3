//> using options -preview
package scala // @preview in private[scala]
import scala.annotation.internal.preview

@preview def previewDef: Int = 42

class Foo:
  def foo: Int = previewDef

class Bar:
  def bar: Int = previewDef
object Bar:
  def bar: Int = previewDef

object Baz:
  def bar: Int = previewDef

def toplevelMethod: Int = previewDef
