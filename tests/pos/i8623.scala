
trait QC:
  object tasty:
    type Tree
    extension (tree: Tree)
      def pos: Tree = ???

def test1 =
  given QC = ???
  def unseal(using qctx: QC): qctx.tasty.Tree = ???
  unseal.pos

def test2 =
  given QC()
  def unseal(using qctx: QC): qctx.tasty.Tree = ???
  unseal.pos

