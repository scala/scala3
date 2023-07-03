// Like widen2, but using a.type only, meaning it should typecheck
import scala.util.Random
val x = 42

type IsX[T] =
    T match
        case x.type => true
        case _ => false

def bothXOrNot(a: Int, b: Int)(using IsX[a.type] =:= IsX[a.type]) = ???

def test = bothXOrNot(Random.nextInt(), Random.nextInt())
