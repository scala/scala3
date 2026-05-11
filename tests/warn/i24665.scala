//> using options -Werror -Wunused:all

def method[A](f: [g[_]] => Unit => g[A]) = ()

@main def main = method([g[_]] => (_: Unit) => null.asInstanceOf[g[Int]])
