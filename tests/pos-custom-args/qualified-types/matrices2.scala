type Pos = {v: Int with v >= 0}

class CheckedArray(val size: Pos):
  private val data = new Array[Double](size)
  def apply(i: Pos with i < size): Double = data(i)
  def update(i: Pos with i < size, x: Double): Unit = data(i) = x

object CheckedArray:
  def ofDim(size: Pos): {arr: CheckedArray with arr.size == size} =
    CheckedArray(size).runtimeChecked

inline def forRange(from: Pos, to: Pos with to >= from)(body: {i: Pos with from <= i && i < to} => Unit): Unit =
  var i: Int = from
  while i < to do
    body(i.runtimeChecked)
    i = i + 1

case class Matrix(width: Pos, height: Pos):
  private val size = width * height
  private val data = CheckedArray.ofDim(size.runtimeChecked)

  def index(i: Pos with i < height, j: Pos with j < width): {res: Pos with res < data.size} =
    (i * width + j).runtimeChecked

  def apply(i: Pos with i < height, j: Pos with j < width): Double =
    data(index(i, j))

  def update(i: Pos with i < height, j: Pos with j < width, x: Double): Unit =
    data(index(i, j)) = x

  def transpose(): {v: Matrix with v.width == height && v.height == width} =
    val transposed = Matrix.ofDim(width = height, height = width)
    forRange(0, height): i =>
      forRange(0, width): j =>
        transposed(j, i) = this(i, j)
    transposed

  def mul(that: Matrix with that.height == width):
    {v: Matrix with v.width == that.width && v.height == height}
  =
    val result = Matrix.ofDim(width = that.width, height = height)
    forRange(0, height): i =>
      forRange(0, that.width): j =>
        var sum = 0.0
        forRange(0, width): k =>
          sum = sum + this(i, k) * that(k, j)
        result(i, j) = sum
    result

object Matrix:
  def ofDim(width: Pos, height: Pos): {m: Matrix with m.width == width && m.height == height} =
    Matrix(width, height).runtimeChecked

@main def Test =
  val m1 = Matrix.ofDim(2, 3)
  val m2 = Matrix.ofDim(3, 2)
  m1.mul(m2)

  val m1T = m1.transpose()
  m1T.mul(m1)
