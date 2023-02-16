def test1 = { (y: Int) => y + 1 }.apply(???)

class C:
  def x: Int = 8

def test2 = { (c: C) => c.x }.apply(null)
