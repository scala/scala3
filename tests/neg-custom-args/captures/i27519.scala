import language.experimental.separationChecking
import caps.*

final class Tree extends Mutable:
  var root: Int = 0
class TM(tree: Tree^) extends Mutable:
  def get: Int = tree.root
  update def add(x: Int): Unit = tree.root = x
def write(m: TM^{any.rd}): Unit =
  val p = m
  p.add(1)  // error
  m.add(1)  // error
def write2(m: TM^): Unit =
  val p = m
  p.add(1)  // ok
  m.add(1)  // ok
