package tests.genericMethods

class Types:
  def orTypes(base: Int | String, nested: List[Int | Double] | Seq[String]): Unit
  = ???
  def andTypes(base: Int & String, nested: List[Int & Double] & Seq[String]): Unit
  = ???

  def mixedAndAndOr(base: Int & String, nested: List[Int | Double] & Seq[String]): Unit
  = ???

  def literal(i: 1, d: 3.3d, c: 'c'): 34
  = 34

  def byName(a: => Int, b: => String | Int): Unit
  = ???