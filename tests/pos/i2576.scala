class Bar(using x: Int)(y: String)
given Int = ???
def test = new Bar("")
