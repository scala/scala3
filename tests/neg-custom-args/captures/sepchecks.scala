import caps.Mutable
import caps.any


trait Rdr[T]:
  def get: T

class Ref[T](init: T) extends Rdr[T], Mutable:
  private var current = init
  def get: T = current
  update def put(x: T): Unit = current = x

def Test(c: Object^): Unit =
  val a: Ref[Int]^ = Ref(1)
  val b: Ref[Int]^ = Ref(2)
  def aa = a

  val getA = () => a.get
  val _: () ->{a.rd} Int = getA

  val putA = (x: Int) => a.put(x)
  val _: Int ->{a} Unit = putA

  def setMax(x: Ref[Int]^{any.rd}, y: Ref[Int]^{any.rd}, z: Ref[Int]^{any}) =
    val doit = () => z.put(x.get max y.get)
    val _: () ->{x.rd, y.rd, z} Unit = doit
    doit()

  def setMax2(x: Rdr[Int]^{any.rd}, y: Rdr[Int]^{any.rd}, z: Ref[Int]^{any}) = ???

  setMax2(aa, aa, b)
  setMax2(a, aa, b)
  setMax2(a, b, b) // error
  setMax2(b, b, b) // error

  abstract class IMatrix:
    def apply(i: Int, j: Int): Double

  class Matrix(nrows: Int, ncols: Int) extends IMatrix, Mutable:
    val arr = Array.fill(nrows, ncols)(0.0)
    def apply(i: Int, j: Int): Double = arr(i)(j)
    update def update(i: Int, j: Int, x: Double): Unit = arr(i)(j) = x

  def mul(x: IMatrix^{any.rd}, y: IMatrix^{any.rd}, z: Matrix^): Matrix^ = ???

  val m1 = Matrix(10, 10)
  val m2 = Matrix(10, 10)
  mul(m1, m2, m2) // error: will fail separation checking
  mul(m1, m1, m2) // ok

  def move(get: () => Int, set: Int => Unit) =
    set(get())

  val geta = () => a.get

  def get2(x: () => Int, y: () => Int): (Int, Int) =
    (x(), y())

  move(geta, b.put(_)) // ok
  move(geta, a.put(_)) // error
  get2(geta, geta) // ok
  get2(geta, () => a.get) // ok
