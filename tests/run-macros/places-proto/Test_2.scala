
@main def Test: Unit =
  var x = 1
  increment(x)
  println(x)

  val arr = Array(1, 2, 3)
  increment(arr(0))
  increment(arr(0))
  increment(arr(2))
  println(arr.toList)

  val a = new A
  increment(a(0))
  increment(a(0))
  increment(a(2))
  println(a)


class A:
  private var x = Array(1, 2, 3)
  def apply(i: Int): Int = x(i)
  def update(i: Int, value: Int): Unit = { x(i) = value }
  override def toString(): String = s"A(${x.mkString(", ")})"
