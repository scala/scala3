class List[+T] {
  
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  
  def prepend [U >: T] (x: U): List[U] = new Cons(x, this)
  
  def map[U](f: T => U): List[U] = if (isEmpty) Nil else tail.map(f).prepend(f(head))
  
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head = throw new Error()
  def tail = ???
}

class Cons[T](hd: T, tl: List[T]) extends List[T] {
  def isEmpty = false
  def head = hd
  def tail = tl
}


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
  
  def cl = ((x: Int) => x + 1)
  
  val ints2 = ints map (_ + 1)
}