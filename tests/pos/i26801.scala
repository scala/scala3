class Context(val lang: String)

inline def ctx(using it: Context): Context = it

def test(xs: List[Int])(using Context): List[(Int, String)] =
  xs.map(tid => tid -> ctx.lang)
