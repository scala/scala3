//> using options -Werror

import scala.language.`future-migration`

def f(x: Int*): Unit = ()
def test(xs: List[Int]): Unit = f(xs: _*) // error: migration warning
