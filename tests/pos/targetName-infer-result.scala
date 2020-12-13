import annotation.targetName
enum Tree:
  case Bind(sym: Symbol, body: Tree)

class Symbol

object Test1:
  abstract class TreeAccumulator[X]:
    def app(x: X, tree: Tree): X
    def app(x: X, trees: List[Tree]): X = ???

  val acc = new TreeAccumulator[List[Symbol]]:
    def app(syms: List[Symbol], tree: Tree) = tree match
      case Tree.Bind(sym, body) => app(sym :: syms, body)

object Test2:
  abstract class TreeAccumulator[X]:
    @targetName("apply") def app(x: X, tree: Tree): X
    def app(x: X, trees: List[Tree]): X = ???

  val acc = new TreeAccumulator[List[Symbol]]:
    @targetName("apply") def app(syms: List[Symbol], tree: Tree) = tree match
      case Tree.Bind(sym, body) => app(sym :: syms, body)
