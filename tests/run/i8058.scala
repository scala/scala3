extension (x: Array[Char])
  inline def swap(i: Int, j: Int) : Unit =
    val v = x(i)
    x(i)  = x(j)
    x(j)  = v

@main def Test =
  val a = Array('A','B')
  a.swap(0, 1)
  assert(a.toList == List('B', 'A'))