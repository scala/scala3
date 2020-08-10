inline def label(x: Int, inline g: Int => String): String = g(x)
def f: Int => String = ???
def label2(g: Int) = label(g, f)
