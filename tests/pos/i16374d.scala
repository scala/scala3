inline def inline1(inline f: Int => Int): Int => Int = i => f(1)
inline def inline2(inline f: Int => Int): Int = f(2) + 3
def test: Int = inline2(inline1(2.+))

