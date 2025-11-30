import scala.language.experimental.relaxedLambdaSyntax

@main def Test =
  val list = List(1, 2, 3)

  val three = list
        .collect: case x =>
        (x, x + 1)
        .toMap // error value toMap is not a member of (Int, Int)

  val huh = list
        .collect: x => case y => (y, y + 1) // error expecting case at x
