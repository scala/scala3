import language.experimental.fewerBraces

def Test =

  val xo: Option[Int] = Some(1)

  val y =
    xo.fold:
      22
    .apply: x =>
      x + 1
  println(y)

