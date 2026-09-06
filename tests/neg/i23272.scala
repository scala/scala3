trait Vectoric[V] {
  def components: Array[V => Double]
  def map(a: V)(f: Double => Double): V
}

trait VectoricOps {
  extension [V: Vectoric as v](lhs: V) {
    def map(f: Double => Double): V = v.map(lhs)(f)
    def toArray: Array[Double] = v.components.map(c => c(lhs)) // error // error
  }
}

trait WorkingVectoricOps {
  // leading implicit is not found, so undesired extension map is not constructed
  extension [V](lhs: V)(using v: Vectoric[V]) {
    def map(f: Double => Double): V = v.map(lhs)(f)
    def toArray: Array[Double] = v.components.map(c => c(lhs))
  }
}
