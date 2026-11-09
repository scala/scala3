package p

def test(h: Holder) =
  h.f(h.c)
  h.g[{h.c}](h.c)
