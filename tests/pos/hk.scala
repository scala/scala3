import language.higherKinds

object higherKinded {
  
  type Untyped = Null

  class Tree[-T >: Untyped] {
    type ThisType[-U >: Untyped] <: Tree[U]
    def withString(s: String): ThisType[String] = withString(s)
  }
  
  class Ident[-T >: Untyped] extends Tree[T] {
    type ThisType[-U] = Ident[U]
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