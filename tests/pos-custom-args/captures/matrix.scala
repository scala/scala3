import caps.Mutable
import caps.any

trait Rdr[T]:
  def get: T

class Ref[T](init: T) extends Rdr[T], Mutable:
  private var current = init
  def get: T = current
  update def put(x: T): Unit = current = x

abstract class IMatrix:
  def apply(i: Int, j: Int): Double

class Matrix(nrows: Int, ncols: Int) extends IMatrix, Mutable:
  val arr = Array.fill(nrows, ncols)(0.0)
  def apply(i: Int, j: Int): Double = arr(i)(j)
  update def update(i: Int, j: Int, x: Double): Unit = arr(i)(j) = x


def mul(x: Matrix, y: Matrix, z: Matrix^): Unit = ???
def mul1(x: Matrix^{any.rd}, y: Matrix^{any.rd}, z: Matrix^): Unit = ???

def Test(c: Object^): Unit =
  val m1 = Matrix(10, 10)
  val m2 = Matrix(10, 10)
  mul(m1, m1, m2) // should be ok
  mul(m1, m1, m2) // should be ok

  def f2(): Matrix^ = Matrix(10, 10)

  val i1: IMatrix^{any.rd} = m1
  val i2: IMatrix^{any.rd} = f2()
