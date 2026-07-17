trait Universe {
  type Tree

  type SymTree <: Tree
  type NameTree <: Tree
  type RefTree <: SymTree & NameTree

  type Ident <: RefTree
  type Select <: RefTree
}

object Test extends App {
  val universe: Universe = null
  import universe.*
  def select: Select = ???
  def ident: Ident = ???
  List(select, ident)
}
