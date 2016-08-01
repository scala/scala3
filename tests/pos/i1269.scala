trait Module {
  sealed abstract class Tree

  case class LetL() extends Tree

  object O {
    case class LetR() extends Tree
  }
}

class Patmat(val module: Module) {
  def patmat(tree: module.Tree) = tree match {
    case module.LetL() =>
    case module.O.LetR() =>
  }
}
