object Foo:
  lazy val (a, (b, c), d) = (1, (2, 3), 4)
  def a2: a.type = a
  def b2: b.type = b
  def c2: c.type = c
  def d2: d.type = d
