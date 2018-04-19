import scala.annotation.tailrec
class Context {
  type Tree
}

class Test {
  @tailrec
  final def loop(c: Context)(trees: List[c.Tree]): Boolean =
    loop(c)(trees)
}
