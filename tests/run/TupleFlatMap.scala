@main def Test = {

  println(
    Tuple().flatMap[[t] =>> (t, t)]([t] => (x: t) => (x, x))
  )

  println(
    (1, 2, "x", "d").flatMap[[t] =>> (t, t)]([t] => (x: t) => (x, x))
  )

  println(
    (1, "x").flatMap[[t] =>> EmptyTuple]([t] => (x: t) => Tuple())
  )

  println(
    (1, "x").flatMap[[t] =>> (t, String)]([t] => (x: t) => (x, x.toString + "!"))
  )

}