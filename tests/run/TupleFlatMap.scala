@main def Test = {

  println(
    Tuple().flatMap[EmptyTuple, [t] =>> (t, t)]([t] => (x: t) => (x, x))
  )

  println(
    (1, 2, "x", "d").flatMap[(Int, Int, String, String), [t] =>> (t, t)]([t] => (x: t) => (x, x))
  )

  println(
    (1, "x").flatMap[(Int, String), [t] =>> EmptyTuple]([t] => (x: t) => Tuple())
  )

  println(
    (1, "x").flatMap[(Int, String), [t] =>> (t, String)]([t] => (x: t) => (x, x.toString + "!"))
  )

}
