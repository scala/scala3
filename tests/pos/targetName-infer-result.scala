import annotation.targetName
enum Tree with
  case Bind(sym: Symbol, body: Tree)

class Symbol

object Test1 with
  abstract class TreeAccumulator[X] with
    def app(x: X, tree: Tree): X
    def app(x: X, trees: List[Tree]): X = ???

  val acc = new TreeAccumulator[List[Symbol]] with
    def app(syms: List[Symbol], tree: Tree) = tree match
      case Tree.Bind(sym, body) => app(sym :: syms, body)

object Test2 with
  abstract class TreeAccumulator[X] with
    @targetName("apply") def app(x: X, tree: Tree): X
    def app(x: X, trees: List[Tree]): X = ???

  val acc = new TreeAccumulator[List[Symbol]] with
    @targetName("apply") def app(syms: List[Symbol], tree: Tree) = tree match
      case Tree.Bind(sym, body) => app(sym :: syms, body)
