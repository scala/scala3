//> using options -Werror

import scala.language.`3.4`

def f(x: Int*): Unit = ()
def test(xs: List[Int]): Unit = f(xs: _*) // error: migration warning
