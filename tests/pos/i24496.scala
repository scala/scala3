import scala.language.experimental.relaxedLambdaSyntax

@main def Test =
  val list = List(1, 2, 3)

  val two = list
        .collect: x => (x, x + 1)
        .toMap

  val one = list
        .collect: case x => (x, x + 1)
        .toMap

  //val huh = list
  //      .collect: x => case y => (y, y + 1) correctly errors expecting case at x
