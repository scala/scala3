import reflect.Selectable.reflectiveSelectable
object App extends App {

  trait Rec0[K <: String] {
    private[App] val map: Map[String, Any]
    def get(k: K): Any
  }
  def Rec0(map0: Map[String, Any]) = new Rec0[String] {
    val map = map0
    def get(k: String): Any = map(k)
  }

  type Rec[K <: String, V0] = Rec0[K] { def get(k: K): V0 }
  def field[V](s: String)(v: V): Rec[s.type, V] = Rec0(Map(s -> v)).asInstanceOf[Rec[s.type, V]]

  implicit class RecOps[R <: Rec0[_]](has: R) {
    def +[K1 <: String, V1](that: Rec[K1, V1]): R & Rec[K1, V1] = Rec0(has.map ++ that.map).asInstanceOf[R & Rec[K1, V1]]
  }

  def rec:
    Rec["k", String]
      & Rec["v", Int]
      & Rec["z", Boolean]
    = {
    field("k")("Str") +
      field("v")(0) +
      field("z")(true)
  }
  def res1: String  = rec.get("k") // error: type mismatch
  def res2: Int     = rec.get("v") // error: type mismatch
  def res3: Boolean = rec.get("z") // error: ambiguous

  //  def res4: Boolean = rec.get("nofield")

  println((res1, res2, res3))
}