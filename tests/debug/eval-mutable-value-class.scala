class A(val value: Int) extends AnyVal:
  def +(x: A) = new A(value + x.value)

object Test:
  def main(args: Array[String]): Unit =
    var x: A = new A(1)
    var y: A = new A(1)
    var z: A = new A(1)
    z += new A(1)
    def xx(): A =
      x += new A(1)
      x
    class B:
      def yy(): A =
        y += new A(1)
        y
    val b = new B
    val res = xx() + b.yy() + z
    println(res)
