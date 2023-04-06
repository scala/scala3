import reflect.Selectable.reflectiveSelectable

class C(var x: Int) {
  def foo(): Int = 20
}

class D(var y: Int) {
  def foo(): Int = A.m
}

class Box(var value: {
  def foo(): Int
})

object A:
  val box1: Box = new Box(new C(5))
  val box2: Box = new Box(new D(10))
  val m: Int = box1.value.foo() // error