import scala.language.`3.4`

def f(x: Int*): Unit = ()
def test(xs: List[Int]): Unit = f(xs: _*) // warn: migration warning
