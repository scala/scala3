type Pos = {v: Int with v >= 0}

class CheckedArray(val size: Pos):
  private val data = new Array[Double](size)
  def apply(i: Pos with i < size): Double = data(i)
  def update(i: Pos with i < size, x: Double): Unit = data(i) = x

object CheckedArray:
  def ofDim(size: Pos): {arr: CheckedArray with arr.size == size} =
    CheckedArray(size).runtimeChecked

case class RRange(from: Int, until: Int with from < until):
  def foreach(body: {x: Int with from <= x && x < until} => Unit): Unit =
    var i: Int = from
    while i < until do
      body(i.runtimeChecked)
      i += 1

def range(from: Int, until: Int with from < until): {r: RRange with r == RRange(from, until)} =
  RRange(from, until)

@main def checkedArrayTest(): Unit =
  val a = CheckedArray.ofDim(10)
  val r = range(0, 10)
  for i <- r do
    a(i) = i
