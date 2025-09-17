abstract class Test:
  this: Test^ =>
  def outer: Int

  class Inner(x: Int):
    def this() = this(outer)

  val i = Inner()
  val _: Inner = i

  val f = () => Inner()
  val _: () ->{this} Inner = f

