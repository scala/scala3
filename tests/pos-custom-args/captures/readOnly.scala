import caps.Mutable
import caps.cap

class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  mut def put(x: Int): Unit = current = x

def Test(c: Object^) =
  val a: Ref^ = Ref(1)
  val b: Ref^ = Ref(2)

  val getA = () => a.get
  val _: () ->{a.rd} Int = getA

  val putA = (x: Int) => a.put(x)
  val _: Int ->{a} Unit = putA

  def setMax(x: Ref^{cap.rd}, y: Ref^{cap.rd}, z: Ref^{cap}) =
    val doit = () => z.put(x.get max y.get)
    val _: () ->{x.rd, y.rd, z} Unit = doit
    doit()
