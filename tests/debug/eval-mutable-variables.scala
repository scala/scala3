object Test:
  def main(args: Array[String]): Unit =
    val a = new A
    println(a.m)

class A:
  private var x = 1
  def m: Int =
    var y = 1
    var z = 1
    var u = 1 // not captured
    x += 1
    def yy(): Int =
      y += 1
      y
    class B:
      def zz(): Int =
        z += 1
        z
    val b = new B
    x + yy() + b.zz() + u
