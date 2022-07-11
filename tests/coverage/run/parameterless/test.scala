object O:
  def f: String =
    println("O.f")
    "O.f_res"

  def g[T]: String =
    println("O.g")
    "O.g_res"

@main
def Test: Unit =
  def f: String =
    println("f")
    "f_res"

  def g[T]: String =
    println("g")
    "g_res"

  val x = f
  val y = g
  println(f)
  println(g)
  println(O.f)
  println(O.g)
