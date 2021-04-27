
extension (x: Int) def m (y: Object): Int = x
def m (x: Int)(y: String): String = y * x

@main def Test = println(1.m("xx"))
