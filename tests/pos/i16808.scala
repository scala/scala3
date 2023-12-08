// scalac: -source future -deprecation -Xfatal-warnings

def collectKeys[A, B, C](xs: Map[A, B])(f: PartialFunction[A, C]): Map[C, B] =
	xs.collect{ case (f(c) , b) => (c, b) }
