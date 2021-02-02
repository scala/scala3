import scala.compiletime.erasedValue
import scala.deriving.Mirror

object Test extends App {
  inline def checkElems[A, T](using inline A: Mirror.SumOf[A]): Unit =
    inline erasedValue[A.MirroredElemTypes] match {
      case _: T => ()
    }

  sealed trait Base1 // Base1 MUST NOT have a companion here!
  case class Foo() extends Base1
  case object Bar extends Base1
  case class Qux(i: Int) extends Base1

  checkElems[Base1, (Foo, Bar.type, Qux)]

  enum Tree[+T] {
    case Empty
    case Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]
    case Leaf[T](elem: T) extends Tree[T]
  }

  checkElems[Tree[String], (Tree.Empty.type, Tree.Branch[String], Tree.Leaf[String])]
}
