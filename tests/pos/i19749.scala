import scala.deriving.Mirror

case class A(x: Int, y: String)

trait SomeTrait[T]

object SomeTrait:
  given [T]: SomeTrait[T] with {}

def f1[T](using p: Mirror.ProductOf[T]): Tuple.Elem[p.MirroredElemTypes, 0] = ???

def f2[T, R](f: T => R)(using SomeTrait[R]) = ???

// Scala3.3 is fine, 3.4 has compilation errors, p MirroredElemTypes type is missing and has been changed to Nothing
val x = f2(_ => f1[A])
