transparent inline def f: Null = null

inline def g: Unit =
  inline if f == "V" then 1 else 2
  inline if f != "V" then 3 else 4
  inline if "v" == f then 5 else 6
  inline if "v" != f then 7 else 8

def test = g
