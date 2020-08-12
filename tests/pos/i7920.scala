class Bar {val glass : Int = 0}
class Foo(val bar : Bar) extends scala.reflect.Selectable
val f = new Foo(new Bar) {
  export bar._
}
val fGlass = f.glass