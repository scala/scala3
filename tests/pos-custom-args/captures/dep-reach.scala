import caps.unboxed
object Test:
  class C
  type Proc = () => Unit

  def f(c: C^, d: C^): () ->{c, d} Unit =
    def foo(@unboxed xs: Proc*): () ->{xs*} Unit =
      xs.head
    val a: () ->{c} Unit = () => ()
    val b: () ->{d} Unit = () => ()
    val xx = foo(a, b)
    xx

  def g(c: C^, d: C^): () ->{c, d} Unit =

    def foo(@unboxed xs: Seq[() => Unit]): () ->{xs*} Unit =
      xs.head

    val a: () ->{c} Unit = () => ()
    val b: () ->{d} Unit = () => ()
    val xx = foo(Seq(a, b))
    xx
