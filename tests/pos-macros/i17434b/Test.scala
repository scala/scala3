import NameOf._
def test() =
    def func1(x: Int): String = ???
    val funcVal = func1 _
    assert(nameOf(funcVal) == "funcVal")
    assert(nameOf(func1 _) == "func1")
