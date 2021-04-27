import compiletime.uninitialized
class Test {

  class C { type T; type Coll }

  type T1 = C { type T = Int }

  type T11 = T1 { type Coll = Set[Int] }

  type T2 = C { type Coll = Set[T] }

  type T22 = T2 { type T = Int }

  var x: T11 = uninitialized
  var y: T22 = uninitialized

  x = y
  y = x

}

class Test2 {

  trait A
  trait B

  class C { type T }

  type T1 = C { type T <: A } { type T <: B }

  type U1 = C { type T <: B } { type T <: A }

  var x: T1 = uninitialized
  var y: U1 = uninitialized

  x = y
  y = x
}


class Test3 {

  trait A
  trait B

  class C { type T }

  type T1 = C { type T <: A }
  type T2 = T1 { type T <: B }

  type U1 = C { type T <: B }
  type U2 = U1 { type T <: A }

  var x: T2 = uninitialized
  var y: U2 = uninitialized

  val x1 = x
  val y1 = y

  x = y
  y = x

}
class Test4 {

  abstract class A { type T; val xz: Any }

  val yy: A { val xz: T } = null;
//  val xx: A { val xz: T } = null;
  val zz: A { val xz: T } = yy;

}

