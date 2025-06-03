class A:
  var x: Int = 10

object O:
  val o: A | (Int => Int) = (x: Int) => x + 1
  o match
    case a: A => a.x = 20
    case f: (_ => _) => f.asInstanceOf[Int => Int](5)
