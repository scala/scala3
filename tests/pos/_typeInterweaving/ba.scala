
given String = ""
given Double = 0

def ba[B][A](x: A)(using B): B = summon[B]

def test =
    ba[String](0)