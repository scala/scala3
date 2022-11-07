@nilAnnot // error
def f1 = 1

class B:
  @nilAnnot // error
  def f2 = 2

  def test =
    @nilAnnot // error
    def f3 = 2
    ()
