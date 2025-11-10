inline def f(t0: Int, t1: Int, t2: Int) = {
  inline (t0, t1, t2) match {
    case (a: Int, b: Int, c: Int) => println(s"a = $a, b = $b, c = $c")
  }
}

@main def Test = f(0, 1, 2)
