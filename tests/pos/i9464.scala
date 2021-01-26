trait T:
  type X
  def x: X

def test1(t: T): t.X = t.x
def test2(t: T): t.X = return t.x
