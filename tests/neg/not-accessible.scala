package foo:

  class A(private[A] val x: Int)

  def test(a: A) = a.x // error

  class B:
    def test(a: A) = a.x // error
  object B:
    def test(a: A) = a.x // error

  package bar:
    def test(a: A) = a.x // error

def test(a: foo.A) = a.x // error
