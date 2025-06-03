trait GraphTraversal  {
  type NodeT

  protected trait Properties {
    def root: NodeT
  }

  abstract protected class TraverserMethods[A, +CC <: TraverserMethods[A, CC]]  { this: CC with Properties =>
    def root: NodeT
  }
}