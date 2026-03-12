type Pos = {v: Int with v >= 0}

case class CheckedArray(val size: Pos):
  private val data = new Array[Double](size)
  def apply(i: Pos with i < size): Double = data(i)
  def update(i: Pos with i < size, x: Double): Unit = data(i) = x

case class RRange(from: Int, until: Int with from < until):
  def foreach(body: {x: Int with from <= x && x < until} => Unit): Unit =
    var i: Int = from
    while i < until do
      body(i.runtimeChecked)
      i += 1

@main def checkedArrayTest(): Unit =
  val a = CheckedArray(10)
  val r = RRange(0, 10)
  for i <- r do
    a(i) = i
