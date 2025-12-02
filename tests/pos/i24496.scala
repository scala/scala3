import scala.language.experimental.relaxedLambdaSyntax

@main def Test =
  val list = List(1, 2, 3)

  val three = list
        .collect: case x =>
          val y = x + 1
          (x, y)
        .toMap

  val two = list
        .collect: x => (x, x + 1)
        .toMap

  val one = list
        .collect: case x => (x, x + 1)
        .toMap
