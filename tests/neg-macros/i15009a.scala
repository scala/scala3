import scala.quoted.*
def test(using Quotes): Unit = {
  '[Int] // error
  '[List[${Type.of[Int]}]] // error

  Type.of[Int] match
    case '[List[$a]] => // error

  val int = Type.of[Int]
  '{ List.empty[$int] } // error
  val t: ${int} = ??? // error
  $int // error: Not found: $int
}
