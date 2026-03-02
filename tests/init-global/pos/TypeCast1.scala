class A:
  class B(val b: Int)

object O:
  val o: A | Array[Int] = new Array[Int](10)
  o match
    case a: A => new a.B(10)
    case arr: Array[Int] => arr(5)

