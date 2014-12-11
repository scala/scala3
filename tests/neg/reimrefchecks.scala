import dotty.readonly
import dotty.mutable

object reimrefchecks {
  class Box {
    var field: Int = 42
  }
  class A {
    def m(box: Box @readonly, box2: Box @mutable): Box @mutable = new Box
  }
  class B extends A {
    override def m(box: Box @mutable, box2: Box @readonly): Box @readonly = box
//override def m(box: Box @readonly, box2: Box @mutable): Box @mutable = new Box
  }
}