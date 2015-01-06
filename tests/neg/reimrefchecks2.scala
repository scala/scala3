import dotty.readonly
import dotty.mutable

object reimrefchecks2 {
  class Box {
    var field: Int = 42
  }
  class A {
    @readonly def m(box: Box @readonly, box2: Box @mutable): Box @mutable = new Box
  }
  class C extends A{
    override def m(box: Box @readonly, box2: Box @mutable): Box @mutable = new Box // ERROR 1
  }
}