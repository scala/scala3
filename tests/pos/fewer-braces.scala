import language.`3.3`

def Test =

  val xo: Option[Int] = Some(1)

  val y =
    xo.fold:
      22
    .apply: x =>
      x + 1
  println(y)

