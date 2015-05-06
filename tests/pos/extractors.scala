object test {

  class Tree
  class Apply(val fun: Tree, val args: List[Tree]) extends Tree

  trait DeconstructorCommon[T >: Null <: AnyRef] {
    var field: T = null
    def get: this.type = this
    def isEmpty: Boolean = field eq null
    def isDefined = !isEmpty
    def unapply(s: T): this.type ={
      field = s
      this
    }
  }

  trait ApplyDeconstructor extends DeconstructorCommon[Apply] {
    def _1: Tree
    def _2: List[Tree]
  }

  object Apply extends ApplyDeconstructor {
    def _1: Tree = field.fun
    def _2: List[Tree] = field.args
  }

  def assocsFromApply(tree: Tree) = {
    tree match {
      case Apply(fun, args) => ???
    }
  }
}
