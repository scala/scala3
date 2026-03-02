type Pos = {v: Int with v >= 0}

class CheckedArray(val size: Pos):
  private val data = new Array[Double](size)
  def apply(i: Pos with i < size): Double = data(i)
  def update(i: Pos with i < size, x: Double): Unit = data(i) = x

object CheckedArray:
  def ofDim(size: Pos): {arr: CheckedArray with arr.size == size} = CheckedArray(size).runtimeChecked

case class Matrix(width: Pos,height: Pos):
  private val data = CheckedArray.ofDim((width * height).runtimeChecked)

  def apply(i: Pos with i < height, j: Pos with j < width): Double =
    data((i * width + j).runtimeChecked)

  def transpose(): {v: Matrix with v.width == height && v.height == width} =
    val transposed = Matrix.ofDim(width = height, height = width)
    for i <- 0 until height do
      for j <- 0 until width do
        transposed.data((j * height + i).runtimeChecked) =
          data((i * width + j).runtimeChecked)
    transposed

  def mul(that: Matrix with that.width == height):
    {v: Matrix with v.width == that.height && v.height == height}
  =
    val result = Matrix.ofDim(width = that.height, height = height)
    for i <- 0 until height do
      for j <- 0 until that.height do
        var sum = 0.0
        for k <- 0 until width do
          sum = sum + this(i.runtimeChecked, k.runtimeChecked) * that(k.runtimeChecked, j.runtimeChecked)
        result.data((i * that.height + j).runtimeChecked) = sum
    result

object Matrix:
  def ofDim(width: Pos, height: Pos): {m: Matrix with m.width == width && m.height == height} =
    Matrix(width, height).runtimeChecked

@main def Test =
  val m1 = Matrix.ofDim(2, 3)
  val m2 = Matrix.ofDim(3, 2)
  //m1.mul(m2)
  //
  //val m1T = m1.transpose()
  //m1T.mul(m1)

