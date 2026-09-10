// https://github.com/epfl-lara/dotty/issues/1

type Pos = {v: Int with v >= 0}
type Dim = {v: Int with v >= 0}

class CheckedArray(val size: Pos):
  private val data = new Array[Double](size)
  def apply(i: Pos with i < size): Double = data(i)
  def update(i: Pos with i < size, x: Double): Unit = data(i) = x

object CheckedArray:
  def ofDim(size: Pos): {res: CheckedArray with res.size == size} =
    CheckedArray(size).asInstanceOf[{res: CheckedArray with res.size == size}]

case class Matrix(width: Dim, height: Dim):
  private val size = width * height
  private val data = CheckedArray.ofDim(size.asInstanceOf[Pos])
