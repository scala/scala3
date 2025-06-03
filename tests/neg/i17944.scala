package test {

  import types._

  object types {
    opaque type ->>[K, V] = V
    extension [K <: Singleton](k: K) def ->>[V](v: V): K ->> V = v.asInstanceOf[K ->> V]
  }

  type FindField[T <: Tuple, K] = FindField0[T, K, 0]

  type FindField0[T <: Tuple, K, I <: Int] <: (Any, Int) = T match {
    case (K ->> f) *: _ => (f, I)
    case _ *: t => FindField0[t, K, compiletime.ops.int.S[I]]
  }

  trait Selector[T, Key, Out] {
    def apply(t: T): Out
  }

  object Selector {
    inline def selectorInst[T <: Tuple, K](
      using idx: ValueOf[Tuple.Elem[FindField[T, K], 1]],
    ): Selector[T, K, Tuple.Head[FindField[T, K]]] =
      new Selector[T, K, Tuple.Head[FindField[T, K]]] {
        def apply(t: T): Tuple.Head[FindField[T, K]] =
          val i: Int = idx.value.asInstanceOf[Int]
          t.productElement(i).asInstanceOf[Tuple.Head[FindField[T, K]]]
      }
  }

}

object Test {
  def main(args: Array[String]): Unit = {
    import test._
    import test.types._

    val t = ("s" ->> "foo") *: ("i" ->> 3) *: EmptyTuple
    val s = Selector.selectorInst[("s" ->> String) *: ("i" ->> Int) *: EmptyTuple, "i"] // error
    val r = s(t)
    println(r)
  }
}
