sealed trait Tree
case class Ident(name: String) extends Tree

object Ident1 {
  def unapply(tree: Tree): Ident = ???
}

trait Cap
object Ident2 {
  def unapply(tree: Tree)(implicit any: Cap): Ident = ???
}

object Ident3 {
  def unapply(tree: Tree)(implicit any: Cap): Option[Ident] = ???
}



class Test {
  def foo(t: Tree): Unit = t match {
    case Ident1(t) =>
  }

  def bar(t: Tree)(implicit c: Cap): Unit = t match {
    case Ident2(t) =>
  }

  def qux(t: Tree)(implicit c: Cap): Unit = t match {
    case Ident3(t) =>
  }

}