import scala.language.future

def f(x: Int*): Unit = ()
def test(xs: List[Int]): Unit = f(xs: _*) // error: migration error
