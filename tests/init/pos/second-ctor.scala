class Base {
  private var topId: Int = 10

  def this(x: Int) = {
    this()
    println(topId)
  }
}

class Child extends Base(5)