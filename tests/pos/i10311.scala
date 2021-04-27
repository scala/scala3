object Module {
  class MyInt(private val x: Int, private val y: Int)
  object MyInt {
    implicit class Ops(self: MyInt) extends AnyVal {
      def x: Int = self.x
    }
    extension (self: MyInt) def y: Int = self.y
  }
}
object test:
  import Module.*

  val a = new MyInt(42, 43)
  val b = a.x
  val c = a.y
