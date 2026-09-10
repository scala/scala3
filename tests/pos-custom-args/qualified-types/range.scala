type Pos = {v: Int with v >= 0}

trait CheckedArray[T]:
  def size: Pos
  def apply(i: Pos with i < size): T

case class CheckedRange(from: Int, until: Int with until > from):
  def foreach(body: (x: Int with from <= x && x < until) => Unit): Unit =
    foreachLoop(from, body)

  @annotation.tailrec
  private def foreachLoop(
    x: Int with x >= from && x < until,
    body: (x: Int with from <= x && x < until) => Unit
  ): Unit =
    body(x)
    val next = x + 1
    if from <= next && next < until then foreachLoop(next, body)

def checkedArrayTest[T](a: CheckedArray[T] with a.size == 10): Unit =
  val r = CheckedRange(0, 10)
  for i <- r do
    a(i)
