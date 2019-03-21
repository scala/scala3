package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer
import dotty.tools.dotc.tastyreflect
import scala.annotation.tailrec
import java.lang.StringBuilder

import com.vladsch.flexmark.util.ast.Node
import com.vladsch.flexmark.util.html.{FormattingAppendable, FormattingAppendableImpl}
import com.vladsch.flexmark.formatter.MarkdownWriter
import com.vladsch.flexmark.util.sequence.BasedSequence


/** Parser from Tasty files to Flexmark AST
*/
class TastyParser {

  class RootNode (val myRootNode: String) extends Node {
    override def getSegments() =  {
        Node.EMPTY_SEGMENTS
    }
  }


  def parse(reflect: Reflection)(root: reflect.Tree) : Node = {
    import reflect._


    def traverse(child: reflect.Tree, out: FormattingAppendable) : Unit = {
        out.append("test```").line()
    }


    val out = new FormattingAppendableImpl(FormattingAppendable.SUPPRESS_TRAILING_WHITESPACE | FormattingAppendable.COLLAPSE_WHITESPACE)
    traverse(root, out)
    out.getText()
  }
}