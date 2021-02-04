class Test {

  var field: Int = compiletime.uninitialized

  def foo() = {

    var x: Int = 1
    var y: String = "abc"
    @volatile var vx: Double = 2
    @volatile var vo: Exception = null
    var xs: Array[Int] = Array(1, 2, 3)
    val xs1: Object = xs

    def inner() = {
      field = x
      x = x + 1 + field
      y += "d"
      vx = x * 2
      vo = vo
      xs(0) = xs(1)
      xs = xs.clone
    }
  }
}

