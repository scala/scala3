object S

class Context(val lang: String):
  val s: S.type = S

inline def ctx(using it: Context): Context = it

def test(xs: List[Int])(using Context) = xs.map(tid => tid -> ctx.s)
