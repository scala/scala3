import caps.Mutable
import caps.cap

trait Rdr[T]:
  def get: T

class Ref[T](init: T) extends Rdr[T], Mutable:
  private var current = init
  def get: T = current
  mut def put(x: T): Unit = current = x

def Test(c: Object^) =
  val a: Ref[Int]^ = Ref(1)
  val b: Ref[Int]^ = Ref(2)
  def aa = a

  val getA = () => a.get
  val _: () ->{a.rd} Int = getA

  val putA = (x: Int) => a.put(x)
  val _: Int ->{a} Unit = putA

  def setMax(x: Ref[Int]^{cap.rd}, y: Ref[Int]^{cap.rd}, z: Ref[Int]^{cap}) =
    val doit = () => z.put(x.get max y.get)
    val _: () ->{x.rd, y.rd, z} Unit = doit
    doit()

  def setMax2(x: Rdr[Int]^{cap.rd}, y: Rdr[Int]^{cap.rd}, z: Ref[Int]^{cap}) = ???

  setMax2(aa, aa, b)
  setMax2(a, aa, b)

  abstract class IMatrix:
    def apply(i: Int, j: Int): Double

  class Matrix(nrows: Int, ncols: Int) extends IMatrix, Mutable:
    val arr = Array.fill(nrows, ncols)(0.0)
    def apply(i: Int, j: Int): Double = arr(i)(j)
    mut def update(i: Int, j: Int, x: Double): Unit = arr(i)(j) = x

  def mul(x: IMatrix^{cap.rd}, y: IMatrix^{cap.rd}, z: Matrix^) = ???

  val m1 = Matrix(10, 10)
  val m2 = Matrix(10, 10)
  mul(m1, m2, m2) // will fail separation checking
  mul(m1, m1, m2) // ok
