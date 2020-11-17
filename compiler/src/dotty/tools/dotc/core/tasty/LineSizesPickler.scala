package dotty.tools
package dotc
package core
package tasty

import dotty.tools.tasty.TastyFormat.SOURCE
import dotty.tools.tasty.TastyBuffer
import TastyBuffer._

import ast._
import ast.Trees._
import ast.Trees.WithLazyField
import util.{SourceFile, NoSource}
import core._
import Contexts._, Symbols._, Annotations._, Decorators._
import collection.mutable
import util.Spans._

class LineSizesPickler(pickler: TastyPickler) {

  import ast.tpd._
  val buf: TastyBuffer = new TastyBuffer(5000)
  pickler.newSection("LineSizes", buf)

  def pickleLineNumbers(source: SourceFile): Unit =
    val content = source.content()
    var lastIndex = content.indexOf('\n', 0)
    buf.writeInt(lastIndex) // size of first line
    while lastIndex != -1 do
      val nextIndex = content.indexOf('\n', lastIndex + 1)
      val end = if nextIndex != -1 then nextIndex else content.length
      buf.writeInt(end - lastIndex - 1) // size of the next line
      lastIndex = nextIndex

}
