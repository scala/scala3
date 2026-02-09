//> using options -source future -deprecation -Werror

def collectKeys[A, B, C](xs: Map[A, B])(f: PartialFunction[A, C]): Map[C, B] =
	xs.collect{ case (f(c) , b) => (c, b) }
