import caps.Mutable
import caps.any

class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def put(x: Int): Unit = current = x

def Test(c: Object^) =
  val a: Ref^ = Ref(1)
  val b: Ref^ = Ref(2)

  val getA = () => a.get
  val _: () -> Int = getA // error

  val putA = (x: Int) => a.put(x)
  val _: Int -> Unit = putA // error

  def setMax(x: Ref^{any.rd}, y: Ref^{any.rd}, z: Ref^{any.rd}) =
    val doit = () => z.put(x.get max y.get) // error
    val _: () ->{x.rd, y.rd, z} Unit = doit
    doit()
