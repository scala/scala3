import language.higherKinds

object hk0 {

  trait Lambda[type Elem]

  abstract class Functor[F <: Lambda] {
    def map[A, B](f: A => B): F[Elem = A] => F[Elem = B]
  }

  object test1 {
    class ListT[T] extends Lambda[T]

    val ml: Functor[ListT] = ???
    val mx = ml
    var xs: ListT[Int] = ???
    var ys: ListT { type Elem = Int } = xs
    xs = ys
    val mm: (Int => Boolean) => ListT[Int] => ListT[Boolean] = mx.map[Int, Boolean]
    val mm2: (Int => Boolean) => ListT[Int] => ListT[Boolean] = mx.map
  }
}


object higherKinded {

  type Untyped = Null

  class Tree[type -Attr >: Untyped] {
    type ThisType <: Tree
    def withString(s: String): ThisType[Attr = String] = withString(s)
  }
/*
  class Ident[-Attr >: Untyped] extends Tree[Attr] {
    type ThisType = Ident
  }

  val id = new Ident[Integer]

  val y = id.withString("abc")

  val z: Ident[String] = y

  val zz: tpd.Tree = y

  abstract class Instance[T >: Untyped] {g
    type Tree = higherKinded.Tree[T]
  }

  object tpd extends Instance[String]

  def transform(tree: Tree[String]) = {
    val tree1 = tree.withString("")
    tree1: Tree[String]
  }
*/
}

