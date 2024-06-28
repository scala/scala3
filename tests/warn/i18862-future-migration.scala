import scala.language.`future-migration`

def f(x: Int*): Unit = ()
def test(xs: List[Int]): Unit = f(xs: _*) // warn: migration warning
