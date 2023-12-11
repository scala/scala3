object NodeOrdering:
  def postOrderNumbering[NodeType](cfgEntry: NodeType, expand: NodeType => Iterator[NodeType]): Map[NodeType, Int]  = ???

trait CfgNode
trait Method extends CfgNode

def postOrder =
  def method: Method = ???
  def expand(x: CfgNode): Iterator[CfgNode] = ???
  NodeOrdering.postOrderNumbering(method, expand)
