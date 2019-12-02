
trait CompilerInterface {
  type Tree
  type Term <: Tree
}

class Reflect(val internal: CompilerInterface) {

  opaque type Tree = internal.Tree
  opaque type Term <: Tree = internal.Term

  object Tree {
    given Ops: (tree: Tree) {
      def show: String = ???
    }
  }

}

object App {
  val refl: Reflect = ???
  import refl._

  val tree: Tree = ???
  tree.show

  val term: Term = ???
  tree.show

}
