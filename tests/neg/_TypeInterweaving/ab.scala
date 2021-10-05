
given String = ""
given Double = 0

def ab[A][B](x: A)(using B): B = summon[B]

def test =
    ab[Int](0: Int) // error