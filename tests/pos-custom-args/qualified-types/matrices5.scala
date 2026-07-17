type Pos = {v: Int with v >= 0}

case class CheckedArray(size: Pos):
  private val data = new Array[Double](size)
  def apply(i: Pos with i < size): Double = data(i)
  def update(i: Pos with i < size, x: Double): Unit = data(i) = x

def forRange(from: Pos, to: Pos with to >= from)(
  body: (i: Pos with from <= i && i < to) => Unit
): Unit =
  if from < to then forRangeLoop(from, to, from, body)

@annotation.tailrec
def forRangeLoop(
  from: Pos,
  to: Pos with to > from,
  x: Pos with from <= x && x < to,
  body: (i: Pos with from <= i && i < to) => Unit,
): Unit =
  body(x)
  if from <= x + 1 && x + 1 < to then
    forRangeLoop(from, to, x + 1, body)

case class Matrix(width: Pos, height: Pos):
  private val size: Pos = (width * height).runtimeChecked
  private val data = CheckedArray(size)

  def index(i: Pos with i < height, j: Pos with j < width): {res: Pos with res < data.size} =
    (i * width + j).runtimeChecked

  def apply(i: Pos with i < height, j: Pos with j < width): Double =
    data(index(i, j))

  def update(i: Pos with i < height, j: Pos with j < width, x: Double): Unit =
    data(index(i, j)) = x

  def transpose(): {v: Matrix with v.width == height && v.height == width} =
    val res = Matrix(height, width)
    forRange(0, height): i =>
      forRange(0, width): j =>
        res(j, i) = this(i, j)
    res

  def mul(that: Matrix with that.height == width):
    {v: Matrix with v.width == that.width && v.height == height}
  =
    val res = Matrix(that.width, height)
    forRange(0, height): i =>
      forRange(0, that.width): j =>
        var sum = 0.0
        forRange(0, width): k =>
          sum = sum + this(i, k) * that(k, j)
        res(i, j) = sum
    res

def example3(w: Pos, h: Pos) =
  val m1 = Matrix(w, h)
  val m2 = Matrix(h, w)
  m1.mul(m2)

  val m1T = m1.transpose()
  m1T.mul(m1)
