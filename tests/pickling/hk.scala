import language.higherKinds

object hk0 {

  abstract class Base {
    type Rep[T]
    val strRep: Rep[String]
  }

  class Sub extends Base {
    type Rep[T] = T
    val strRep = "abc"
    val sr: Rep[String] = ""
  }

  abstract class Functor[F[_]] {
    def map[A, B](f: A => B): F[A] => F[B]
  }
  val ml: Functor[List] = ???
  val mx = ml
  val mm: (Int => Boolean) => List[Int] => List[Boolean] = mx.map
}

object higherKinded {

  type Untyped = Null

  class Tree[-T >: Untyped] {
    type ThisType[-U >: Untyped] <: Tree[U]
    def withString(s: String): ThisType[String] = withString(s)
  }

  class Ident[-T >: Untyped] extends Tree[T] {
    type ThisType[-U >: Untyped] = Ident[U]
  }

  val id = new Ident[Integer]

  val y = id.withString("abc")

  val z: Ident[String] = y

  val zz: tpd.Tree = y

  abstract class Instance[T >: Untyped] {
    type Tree = higherKinded.Tree[T]
  }

  object tpd extends Instance[String]

  def transform(tree: Tree[String]) = {
    val tree1 = tree.withString("")
    tree1: Tree[String]
  }

}
