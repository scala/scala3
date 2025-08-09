import caps.use
object Test:
  class C
  type Proc[CS^] = () ->{CS} Unit

  def f(c: C^, d: C^): () ->{c, d} Unit =
    def foo[CS^](xs: Proc[CS]*): () ->{CS} Unit =
      xs.head
    val a: () ->{c} Unit = () => ()
    val b: () ->{d} Unit = () => ()
    val xx = foo(a, b)
    xx

  def g(c: C^, d: C^): () ->{c, d} Unit =

    def foo[CS^](xs: Seq[() ->{CS} Unit]): () ->{CS} Unit =
      xs.head

    val a: () ->{c} Unit = () => ()
    val b: () ->{d} Unit = () => ()
    val xx = foo(Seq(a, b))
    xx
