trait Test {
  trait Global {
    type Tree
    def get: Tree
  }

  trait TreeBuilder {
    val global: Global
    inline def set(tree: global.Tree) = {}
  }

  val nsc: Global

  trait FileImpl {
    object treeBuilder extends TreeBuilder {
      val global: nsc.type = nsc
    }
    treeBuilder.set(nsc.get)
  }
  def file: FileImpl

  file.treeBuilder.set(nsc.get)
}
