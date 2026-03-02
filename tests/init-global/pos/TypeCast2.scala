class A:
  class B(val b: Int)

object O:
  val o: A | (Int => Int) = (x: Int) => x + 1
  o match
    case a: A => new a.B(10)
    case f: (_ => _) => f.asInstanceOf[Int => Int](5)

