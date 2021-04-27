class D[T]

class C {
  def f() = {
    locally {
      class dd[U] extends D[U] {
        val xx = 1
      }
      class ee[V] extends dd[(V, V)]
      def d[V]: dd[V] = new dd[V]
      g[D[Int]](d[Int])
      g[D[(Int, Int)]](new ee[Int])
    }
  }

  inline def locally[T](inline body: T): T = body

  def g[T](x: T): T = x
}