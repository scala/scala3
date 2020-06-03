class Foo {
  object MyEnum {
    class Blue
  }
  export MyEnum._

  val a: MyEnum.Blue = ???
  a : Blue   // ok
}

object Test {
  object Types {
    type T <: AnyRef
    type U = T
    type TC[X] <: AnyRef
    type UC[X] = TC[(X, X)]
    class C
    class D[X] extends C
    def x1: T = ???
    def x2: U = ???
    def x3: TC[Int] = ???
    def x4: UC[Int] = ???
    def x5: C = C()
    def x6: D[Int] = D()
  }
  export Types._
  type D1[X] = Types.D[X]
  type U1[X] = Types.UC[X]

  val y1: T = x1
  val y2: U = x2
  val y3: TC[Int] = x3
  val y4: UC[Int] = x4
  val y5: C = x5
  val y6: D[Int] = x6
}
