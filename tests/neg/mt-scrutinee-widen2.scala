// A test case showing how we shouldn't widen
// both IsX scrutinees and make "def test" typecheck
import scala.util.Random
val x = 42

type IsX[T] =
    T match
        case x.type => true
        case _ => false

def bothXOrNot(a: Int, b: Int)(using IsX[a.type] =:= IsX[b.type]) = ???

def test = bothXOrNot(Random.nextInt(), Random.nextInt()) // error
