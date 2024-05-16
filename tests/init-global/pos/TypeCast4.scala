class A:
  var x: Int = 10

object O:
  val o: A | Array[Int] = new Array[Int](10)
  o match
    case a: A => a.x = 20
    case arr: Array[Int] => arr(5)

