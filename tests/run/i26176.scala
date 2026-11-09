class C:
  private def secret = 42
  def foo = 10
  inline def m = new C { override def foo = secret }

@main def Test =
  val e = new C().m
  assert(e.foo == 42)
