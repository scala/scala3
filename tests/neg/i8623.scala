
trait QC:
  object tasty:
    type Tree
    extension (tree: Tree)
      def pos: Tree = ???

def test =
  given [T]: QC = ???
  def unseal(using qctx: QC): qctx.tasty.Tree = ???
  unseal.pos  // error

