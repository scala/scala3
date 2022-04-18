import language.postfixOps as _

extension (sc: StringContext) def sum(xs: Int*): String = xs.sum.toString

def test = sum"${ List(42)* }" // error // error

