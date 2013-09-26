class List[+T] {
  
  def prepend [U >: T] (x: U): List[U] = null//new Cons(x, this)
  
  def map[U](f: T => U): List[U] = null
  
}

object Nil extends List[Nothing]

//class Cons[T](hd: T, tl: List[T]) extends List[T]


object Inferred {

  def foo[T](x: T): T = x

  val x = foo(1)

  val y = foo("abc")

  def bar[U](xs: List[U]): List[U] = xs

  val n = Nil

  val nn = bar(Nil)

  val ints: List[Int] = Nil prepend 1

  val a = if (1 == 0) Nil else ints
  
  val n2 = scala.collection.immutable.Nil
  
  val ss2: scala.collection.immutable.List[String] = "abc" :: n2
  
  val ss3 = "abc" :: n2
  
  def closure = ((x: Int) => x)
  
  val ints2 = ints map closure
}