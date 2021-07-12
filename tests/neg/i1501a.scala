
object Test2 {
  class A
  class SubA(x: Int) extends A
  trait TA extends A
  trait TSubA extends SubA(2) // error: trait TSubA may not call constructor of class SubA


  class Foo extends TA with TSubA // error: missing argument for parameter x of constructor SubA:
}
