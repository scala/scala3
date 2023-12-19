//> using options -Wnonunit-statement

class Node()
class Elem(
  prefix: String,
  label: String,
  minimizeEmpty: Boolean,
  child: Node*
) extends Node
class Text(text: String) extends Node
class NodeBuffer() {
  def &+(node: Node): NodeBuffer =
    this
}
class NodeSeq()
object NodeSeq {
  def seqToNodeSeq(seq: NodeBuffer): Seq[Node] = ???
}

object Main {
  def example() = {
    {
      new Elem(null, "foo", false,
        {
          val $buf: NodeBuffer = new NodeBuffer()
          $buf.&+(new Text("bar"))
          NodeSeq.seqToNodeSeq($buf)
        }*
      )
    }
  }: @annotation.nowarn()
}