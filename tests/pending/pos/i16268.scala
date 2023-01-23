import language.experimental.captureChecking
class Tree
case class Thicket(trees: List[Tree]) extends Tree

def test1(segments: List[{*} Tree]) =
  val elems = segments flatMap { (t: {*} Tree) => t match // error
    case ts: Thicket => ts.trees.tail
    case t => Nil
  }
  elems

def test2(segments: List[{*} Tree]) =
  val f = (t: {*} Tree) => t match
    case ts: Thicket => ts.trees.tail
    case t => Nil
  val elems = segments.flatMap(f) // error
  elems

def test3(c: {*} Any)(segments: List[{c} Tree]) =
  val elems = segments flatMap { (t: {c} Tree) => t match
    case ts: Thicket => ts.trees.tail
    case t => Nil
  }
  elems

