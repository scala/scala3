import scala.language.experimental.relaxedLambdaSyntax

@main def Test =
  val list = List(1, 2, 3)

  val three = list
        .collect: case x =>
          (x, x + 1) // error not a member of tuple
        .toMap

  val huh = list
        .collect: x => case y => (y, y + 1) // error expecting case at x
