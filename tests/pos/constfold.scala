object A {
  val x = 2
  val y = x.asInstanceOf[Byte]
  val z = 1.0 / 2
  val s = "z is " + z

  val a = 1 + 1
  val b = -(1:1)
  val c = -(1:1 & Any)
}

object Test extends App {

    Console.println(A.x);
    Console.println(A.y);
    Console.println(A.z);
    Console.println(A.s);

    def f(x: 12): Int = 1
    def f(x: Int): Double = 2
    val x = f(12)
    val _: Int = x
    val y = f(2 * 6)
    val _: Int = x

}
