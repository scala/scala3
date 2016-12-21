sealed trait T1 {type M1}

case object o1 extends T1

sealed trait T2 {type M2}

case object o2 extends T2

class TestX {
  type TestT1 <: T1 {type M1 = TestT2}
  type TestT2 <: T2 {type M2 = TestT1}
  //val x: TestT1 = o1
}
