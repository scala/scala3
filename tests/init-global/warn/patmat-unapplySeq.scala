object A:
  class Box(var x: Int)

  val array: Array[Box] = new Array(1)
  array(0) = new Box(10)

  def length: Int = array.length
  def apply(i: Int): Box = array(i)
  def drop(n: Int): Seq[Box] = array.toSeq
  def toSeq: Seq[Box] = array.toSeq

  def unapplySeq(array: Array[Box]): A.type = this


object B:
  A.array match
  case A(b) =>

