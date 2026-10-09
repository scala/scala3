object Test:

  class Container[T](val id: Int, var x: T):

    def y: T =
      println(s"load ${x} from ${id}")
      x

    def y_=(newValue: T): Unit =
      println(s"store ${newValue} to ${id}")
      this.x_=(newValue)

  def main(args: Array[String]): Unit =
    // simple swap
    var x1 = 4
    var x2 = 2
    (x1, x2) = (x2, x1)
    println(s"after swap: (${x1}, ${x2})")

    // swap in a container
    val a = Array(4, 2)
    (a(0), a(1)) = (a(1), a(0))
    println(s"after swap: (${a(0)}, ${a(1)})")

    // swap fields with effectless left-hand sides
    var c1 = Container(1, 4)
    var c2 = Container(2, 2)
    (c1.x, c2.x) = (c2.x, c1.x)
    println(s"after swap: (${c1.x}, ${c2.x})")

    // swap fields with side effectful left-hand sides
    def f(n: Int): Container[Int] =
      println(s"load ${n}")
      n match
        case 1 => c1
        case 2 => c2
    (f(1).y, f(2).y) = (f(2).y, f(1).y)
    println(s"after swap: (${c1.x}, ${c2.x})")
