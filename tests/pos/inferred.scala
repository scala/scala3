abstract class LIST[+T] {

  def isEmpty: Boolean
  def head: T
  def tail: LIST[T]

  def prepend [U >: T] (x: U): LIST[U] = new CONS(x, this)

  def map[U](f: T => U): LIST[U] = if (isEmpty) NIL else tail.map(f).prepend(f(head))

}

object NIL extends LIST[Nothing] {
  def isEmpty = true
  def head = throw new Error
  def tail = ???
}

class CONS[U](hd: U, tl: LIST[U]) extends LIST[U] {
  def isEmpty = false
  def head: U = hd
  def tail = tl
}

object Inferred {

  def foo[T](x: T): T = x

  val x = foo(1)

  val y = foo("abc")

  def bar[U](xs: LIST[U]): LIST[U] = xs

  val n = NIL

  val nn = bar(NIL)

  val ints: LIST[Int] = NIL prepend 1

  val ints1 = NIL prepend 1 prepend 2

  val a = if (1 == 0) NIL else ints

  val n2 = scala.collection.immutable.Nil

  val ss2: scala.collection.immutable.List[String] = "abc" :: n2

  val ss3 = "abc" :: n2

  def cl = ((x: Int) => x + 1)

  val ints2 = ints map (_ + 1)

  val ints3 = new CONS[Int](1, NIL)

  val ints4 = new CONS(1, NIL)
}
