trait Module {
  sealed trait Tree

  case class LetL() extends Tree
  case class LetR() extends Tree
}

class Patmat[T <: Module](val module: T) {
  def patmat(tree: module.Tree) = tree match {
    case module.LetL() => ???
  }

  def exhaust(tree: module.Tree) = tree match {
    case module.LetL() => ???
    case module.LetR() => ???
  }
}

class Patmat2(val module: Module) {
  def patmat(tree: module.Tree) = tree match {
    case module.LetL() => ???
  }

  def exhaust(tree: module.Tree) = tree match {
    case module.LetL() => ???
    case module.LetR() => ???
  }
}
