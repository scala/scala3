import caps.Mutable
class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def put(x: Int): Unit = current = x
