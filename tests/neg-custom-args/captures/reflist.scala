import caps.*

class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y


def par[A, B](x: A^, y: B^): Unit = ()

def Test =
    val xs1: List[Ref^] = Ref() :: Nil
    val h1 = xs1.head
    val t1 = xs1.tail
    par(h1, t1)    // ok, since t1 is pure
    val h2 = t1.head
    par(h1, h2)    // error
